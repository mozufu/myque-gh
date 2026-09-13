{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket, try)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Github.Cli (runCli)
import Myque.Github.Github (discoverGithub, encodePathSegment, getRepository, graphqlPaginated, restSingle)
import Myque.Github.Markers
import Myque.Github.Projection (ProjectionContext (..), projectIssue, renderDisplay)
import Myque.Github.Reconcile (applyIssueDiff, applyParentChange, buildPlan, defaultProjectionQuery, diffIssue, planIsEmpty, reconcile, selectProjection)
import Myque.Github.Source (decodeBlobBatch, withSnapshot)
import Myque.Github.Types
import Myque.Item (Kind (..), State (..), WorkItem (..), newWorkItem)
import Myque.Query (parseQuery)
import Myque.Store (Store (..), initLayout, saveItem, storeItems)
import Myque.Timestamp (parseTimestamp)
import Myque.Uuid (Uuid, parseUuid)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "decodeBlobBatch" $ do
        it "decodes exact framed blobs" $ do
            let oid = T.replicate 40 "a"
            decodeBlobBatch [oid] (B8.pack (T.unpack oid <> " blob 3\nabc\n")) `shouldBe` Right (Map.singleton oid "abc")
        it "rejects truncation and extra frames" $ do
            let oid = T.replicate 40 "a"
            decodeBlobBatch [oid] (B8.pack (T.unpack oid <> " blob 4\nabc\n")) `shouldSatisfy` isLeft
            decodeBlobBatch [] "extra" `shouldSatisfy` isLeft
    describe "path segment encoding" $ do
        it "percent-encodes UTF-8 bytes and every reserved separator" $ do
            map encodePathSegment ["abc", "a b", "foo/bar", "#tag", "硬體", "é", "💚"]
                `shouldBe` ["abc", "a%20b", "foo%2Fbar", "%23tag", "%E7%A1%AC%E9%AB%94", "%C3%A9", "%F0%9F%92%9A"]
    describe "issue markers" $ do
        it "accepts only a leading UUIDv7 header" $ do
            value <- fixtureUuid
            parseIssueIdentity (renderIssueIdentity value <> "body") `shouldBe` Right (Just value)
            parseIssueIdentity ("body\n" <> renderIssueIdentity value) `shouldBe` Right Nothing
        it "rejects duplicate and incomplete identity headers" $ do
            value <- fixtureUuid
            parseIssueIdentity (renderIssueIdentity value <> renderIssueIdentity value) `shouldSatisfy` isLeft
            parseIssueIdentity ("<!-- myque:id=" <> T.pack (show value) <> " -->\n") `shouldSatisfy` isLeft
    describe "PR trailers" $ do
        it "round-trips Unicode CRLF replacement and clear without touching prefix" $ do
            value <- fixtureUuid
            let prefix = "human π\r\nbody\r\n"
            linked <- requireRight (rewritePrLinks prefix (Set.singleton value))
            parsedPrUuids (parsePrLinks linked) `shouldBe` Set.singleton value
            linkedAgain <- requireRight (rewritePrLinks linked (Set.singleton value))
            linkedAgain `shouldBe` linked
            cleared <- requireRight (rewritePrLinks linked Set.empty)
            cleared `shouldBe` prefix
        it "deduplicates uppercase UUIDs" $ do
            value <- fixtureUuid
            let upper = T.toUpper (T.pack (show value))
                body = "<!-- myque:pr-links=github/v1 -->\n<!-- myque:implements=" <> upper <> " -->\n<!-- myque:implements=" <> T.pack (show value) <> " -->\n<!-- myque:pr-links:end -->\n"
            parsedPrUuids (parsePrLinks body) `shouldBe` Set.singleton value
        it "accepts a real trailer after a fenced example under LF and CRLF" $ do
            value <- fixtureUuid
            let body = "Example:\n\n```md\n" <> renderTrailerFor value <> "```\n\nActual metadata:\n\n" <> renderTrailerFor value
                bodies = [body, T.replace "\n" "\r\n" body]
            map (parsedPrUuids . parsePrLinks) bodies `shouldBe` replicate 2 (Set.singleton value)
            map (parsedPrDiagnostics . parsePrLinks) bodies `shouldBe` replicate 2 []
        it "fails closed for multiple real top-level trailers" $ do
            value <- fixtureUuid
            let parsed = parsePrLinks (renderTrailerFor value <> "text\n" <> renderTrailerFor value)
            parsedPrDiagnostics parsed `shouldBe` ["multiple top-level myque PR trailers"]
        it "ignores multiple fenced examples without a managed trailer" $ do
            value <- fixtureUuid
            let fencedSample fence = fence <> "md\n" <> renderTrailerFor value <> fence <> "\n"
                parsed = parsePrLinks (fencedSample "```" <> "text\n" <> fencedSample "~~~")
            parsedPrUuids parsed `shouldBe` Set.empty
            parsedPrDiagnostics parsed `shouldBe` []
        it "ignores fenced malformed markers before a valid real trailer" $ do
            value <- fixtureUuid
            let fenced = "```md\n<!-- myque:pr-links=github/v1 -->\n<!-- myque:implements=not-a-uuid -->\n```\n"
                parsed = parsePrLinks (fenced <> renderTrailerFor value)
            parsedPrUuids parsed `shouldBe` Set.singleton value
            parsedPrDiagnostics parsed `shouldBe` []
        it "keeps marker-like trailers inside valid Markdown fences" $ do
            value <- fixtureUuid
            let managed = renderTrailerFor value
                cases =
                    [ "```md\n" <> managed <> "````\n"
                    , "```md\r\nexample\r\n```not-closed\r\n" <> T.replace "\n" "\r\n" managed <> "```\r\n"
                    , "~~~ md\n" <> managed <> "```\n~~~\n"
                    , "   ```md\n" <> managed <> "   ```\n"
                    ]
            map (parsedPrUuids . parsePrLinks) cases `shouldBe` replicate 4 Set.empty
            map (parsedPrDiagnostics . parsePrLinks) cases `shouldBe` replicate 4 []
        it "does not treat four-space-indented fences as fenced code" $ do
            value <- fixtureUuid
            let body = "    ```md\n" <> renderTrailerFor value
            parsedPrUuids (parsePrLinks body) `shouldBe` Set.singleton value
        it "fails closed for malformed top-level trailers after fenced examples" $ do
            value <- fixtureUuid
            let body = "```md\n<!-- myque:pr-links=github/v1 -->\n<!-- myque:implements=bad -->\n```\n<!-- myque:pr-links=github/v1 -->\n<!-- myque:implements=" <> T.pack (show value) <> " -->\n"
            parsedPrDiagnostics (parsePrLinks body) `shouldBe` ["malformed myque PR trailer candidate at end of body"]
    describe "issue drift" $ do
        it "preserves human labels while replacing managed labels" $ do
            value <- fixtureUuid
            let desired = DesiredIssue value "A" "title" "body" IssueOpen Nothing (Set.fromList ["myque:state:open"])
                actual = GithubIssue 1 1 "N" "bot" "body" "title" IssueOpen Nothing (Set.fromList ["human", "myque:state:done"]) "now" Nothing Nothing
            diffIssue desired actual `shouldBe` [RemoveLabel "myque:state:done", AddLabel "myque:state:open"]
    describe "projection planning" $ do
        it "uses the conservative default and retains trusted projections" $ do
            snapshot <- fixtureSnapshotWithMilestone
            taskId <- fixtureUuid
            milestoneId <- otherUuid
            selected <- defaultSelection snapshot
            plan <- requireRight (buildPlan selected fixtureTarget snapshot (fixtureGithub Map.empty []))
            Map.keys (planIssueCreates plan) `shouldBe` [taskId]
            let milestone = storeById (snapshotStore snapshot) Map.! milestoneId
                retained = fixtureIssue 9 milestoneId milestone
            retainedPlan <- requireRight (buildPlan selected fixtureTarget snapshot (fixtureGithub (Map.singleton milestoneId retained) []))
            any (\(uuid, _, _) -> uuid == milestoneId) (Map.elems (planIssueChanges retainedPlan)) `shouldBe` True
        it "filters first projections while retaining terminal managed items" $ do
            snapshot <- fixtureSnapshot
            openId <- fixtureUuid
            doneId <- otherUuid
            selected <- selectionFor "kind = bug" snapshot
            plan <- requireRight (buildPlan selected fixtureTarget snapshot (fixtureGithub Map.empty []))
            planIssueCreates plan `shouldBe` Map.empty
            let openItem = storeById (snapshotStore snapshot) Map.! openId
                doneItem = storeById (snapshotStore snapshot) Map.! doneId
                existingOpen = fixtureIssue 8 openId openItem
                existingDone = fixtureIssue 9 doneId doneItem
            retainedPlan <- requireRight (buildPlan selected fixtureTarget snapshot (fixtureGithub (Map.fromList [(openId, existingOpen), (doneId, existingDone)]) []))
            map (\(uuid, _, _) -> uuid) (Map.elems (planIssueChanges retainedPlan)) `shouldMatchList` [openId, doneId]
        it "adds filtered-out parents through closure" $ do
            snapshot <- fixtureSnapshotWithParentKinds
            parentId <- fixtureUuid
            childId <- otherUuid
            selected <- defaultSelection snapshot
            selected `shouldBe` Set.singleton childId
            plan <- requireRight (buildPlan selected fixtureTarget snapshot (fixtureGithub Map.empty []))
            Map.keys (planIssueCreates plan) `shouldMatchList` [parentId, childId]
            let childItem = storeById (snapshotStore snapshot) Map.! childId
            planParentChanges plan `shouldBe` Map.singleton childId (renderDisplay snapshot childItem, Just parentId)
        it "does not create terminal unprojected matches" $ do
            snapshot <- fixtureSnapshot
            doneId <- otherUuid
            selected <- selectionFor "state = done" snapshot
            selected `shouldBe` Set.empty
            plan <- requireRight (buildPlan selected fixtureTarget snapshot (fixtureGithub Map.empty []))
            Map.member doneId (planIssueCreates plan) `shouldBe` False
        it "rejects case-colliding managed tag labels before mutation" $ do
            snapshot <- fixtureSnapshotWithTags ["Foo", "foo"]
            selected <- defaultSelection snapshot
            buildPlan selected fixtureTarget snapshot (fixtureGithub Map.empty []) `shouldSatisfy` isLeft
        it "plans removal and replacement of existing parents" $ do
            snapshot <- fixtureSnapshotWithParentKinds
            parentId <- fixtureUuid
            childId <- otherUuid
            selected <- defaultSelection snapshot
            let parentItem = storeById (snapshotStore snapshot) Map.! parentId
                childItem = storeById (snapshotStore snapshot) Map.! childId
                parentIssue = fixtureIssue 7 parentId parentItem
                childIssue = (fixtureIssue 8 childId childItem){githubIssueParent = Just (GithubIssueRef "owner" "repo" 99)}
                issues = Map.fromList [(parentId, parentIssue), (childId, childIssue)]
            replacePlan <- requireRight (buildPlan selected fixtureTarget snapshot (fixtureGithub issues []))
            Map.lookup childId (planParentChanges replacePlan) `shouldBe` Just (renderDisplay snapshot childItem, Just parentId)
            let detachedSnapshot = snapshotWithoutParent snapshot childId
            removePlan <- requireRight (buildPlan selected fixtureTarget detachedSnapshot (fixtureGithub issues []))
            Map.lookup childId (planParentChanges removePlan) `shouldBe` Just (renderDisplay detachedSnapshot childItem{itemParent = Nothing}, Nothing)
        it "applies parent replacement with the child database id" $ do
            parentId <- fixtureUuid
            childId <- otherUuid
            let parent = blankIssue 7 parentId
                child = (blankIssue 8 childId){githubIssueId = 800, githubIssueParent = Just (GithubIssueRef "owner" "repo" 9)}
                github = fixtureGithub (Map.fromList [(parentId, parent), (childId, child)]) []
            calls <- newIORef []
            let client = scriptedClient $ \_ args input -> do
                    modifyIORef' calls (<> [(args, input)])
                    pure (okIncluded Null)
            applyParentChange client fixtureTarget github childId (Just parentId)
            journal <- readIORef calls
            case journal of
                [(args, input)] -> do
                    "/repos/owner/repo/issues/7/sub_issues" `shouldSatisfy` (`elem` args)
                    input `shouldSatisfy` maybe False (\body -> B8.isInfixOf "\"sub_issue_id\":800" body && B8.isInfixOf "\"replace_parent\":true" body)
                _ -> expectationFailure ("unexpected parent mutation calls: " <> show journal)
        it "encodes Unicode managed-label deletion endpoints" $ do
            value <- fixtureUuid
            let desired = DesiredIssue value "A" "title" "body" IssueOpen Nothing Set.empty
                actual = GithubIssue 1 1 "N" "bot" "body" "title" IssueOpen Nothing (Set.singleton "myque:tag:硬體") "now" Nothing Nothing
                client = scriptedClient $ \_ args _ -> do
                    args `shouldSatisfy` elem "/repos/owner/repo/issues/1/labels/myque%3Atag%3A%E7%A1%AC%E9%AB%94"
                    pure (okIncluded Null)
            applyIssueDiff client fixtureTarget 1 desired actual
        it "encodes Unicode canonical source paths by segment" $ do
            snapshot <- fixtureSnapshot
            value <- fixtureUuid
            let item = storeById (snapshotStore snapshot) Map.! value
                source = (snapshotSource snapshot){sourceLinkRepo = Just ("擁有者", "專案"), sourceLinkBranch = Just "功能/硬體"}
                linked = snapshot{snapshotSource = source, snapshotSourcePaths = Map.singleton value ".tasks/硬體 設計.md"}
                desired = projectIssue (ProjectionContext linked fixtureTarget Map.empty) item []
            desiredBody desired `shouldSatisfy` T.isInfixOf "https://github.com/%E6%93%81%E6%9C%89%E8%80%85/%E5%B0%88%E6%A1%88/blob/%E5%8A%9F%E8%83%BD%2F%E7%A1%AC%E9%AB%94/.tasks/%E7%A1%AC%E9%AB%94%20%E8%A8%AD%E8%A8%88.md"
    describe "GitHub transport" $ do
        it "sends JSON writes through stdin and parses included responses" $ do
            let client = scriptedClient $ \_ args input -> do
                    map (`elem` args) ["--method", "PATCH", "--include", "/repos/o/r/issues/1", "--input", "-"] `shouldBe` replicate 6 True
                    input `shouldSatisfy` maybe False (B8.isInfixOf "\"title\":\"fixed\"")
                    pure (okIncluded (object ["value" .= (1 :: Int)]))
            (_, value) <- restSingle client "PATCH" "/repos/o/r/issues/1" (Just (object ["title" .= ("fixed" :: Text)]))
            value `shouldBe` object ["value" .= (1 :: Int)]
        it "rejects malformed included responses and GraphQL errors" $ do
            let badRest = scriptedClient $ \_ _ _ -> pure (CommandResult ExitSuccess "{}" "")
            restFailure <- try (restSingle badRest "GET" "/repos/o/r" Nothing)
            failureCodeOf restFailure `shouldBe` Just 3
            let graph = scriptedClient $ \_ _ _ -> pure (CommandResult ExitSuccess "[{\"errors\":[{\"message\":\"no\"}]}]" "")
            pages <- graphqlPaginated graph "query($endCursor:String){viewer{login}}" fixtureTarget
            pages `shouldSatisfy` (not . null)
        it "rejects repository identity mismatch" $ do
            let client = scriptedClient $ \_ _ _ -> pure (okIncluded (object ["id" .= (7 :: Int), "full_name" .= ("other/repo" :: Text), "archived" .= False, "has_issues" .= True]))
            mismatch <- try (getRepository client fixtureTarget)
            failureCodeOf mismatch `shouldBe` Just 1
    describe "projection CLI" $ do
        it "rejects an invalid project query before GitHub discovery" $ withSystemTempDirectory "myque-gh-invalid-query" $ \root -> do
            initializeRepo root
            writeFile (root </> ".tasks" </> "items" </> "019a10d8-8d48-7b77-a414-f95ab7af31be.md") itemOne
            git root ["add", "."]
            gitEnv root ["commit", "-m", "fixture"]
            result <- runCli ["plan", "--store", root, "--repo", "owner/repo", "--project", "kind ="]
            result `shouldBe` ExitFailure 1
    describe "writer transactions" $ do
        it "rediscovers an existing projection without duplicate writes" $ withWriterSnapshot $ \spec snapshot -> do
            value <- fixtureUuid
            let item = storeById (snapshotStore snapshot) Map.! value
                desired = projectIssue (ProjectionContext snapshot fixtureTarget (Map.singleton value 7)) item []
                issue = issueFromDesired 7 desired
            journal <- newIORef []
            let client = statefulClient journal (const (pure (WriterStable issue)))
            plan <- reconcile (Set.singleton value) client spec fixtureTarget snapshot
            planIsEmpty plan `shouldBe` True
            calls <- readIORef journal
            filter isWriteCall calls `shouldBe` []
        it "converges when issue and label lists lag behind creates" $ withWriterSnapshot $ \spec snapshot -> do
            value <- fixtureUuid
            let item = storeById (snapshotStore snapshot) Map.! value
                desired = projectIssue (ProjectionContext snapshot fixtureTarget (Map.singleton value 7)) item []
                issue = issueFromDesired 7 desired
            journal <- newIORef []
            let client = scriptedClient $ \_ args _ -> do
                    modifyIORef' journal (<> [args])
                    pure (laggingResponse issue args)
            plan <- reconcile (Set.singleton value) client spec fixtureTarget snapshot
            Map.keys (planIssueCreates plan) `shouldBe` [value]
            calls <- readIORef journal
            length (filter (\args -> isWriteCall args && any (isInfixOfArg "/issues") args) calls) `shouldBe` 1
        it "stops parent mutation when the child identity changed" $ withParentWriterSnapshot $ \spec snapshot -> do
            parentId <- fixtureUuid
            childId <- otherUuid
            let store = storeById (snapshotStore snapshot)
                projection = ProjectionContext snapshot fixtureTarget (Map.fromList [(parentId, 7), (childId, 8)])
                desiredFor uuid = projectIssue projection (store Map.! uuid) []
                parentIssue = issueFromDesired 7 (desiredFor parentId)
                childIssue = issueFromDesired 8 (desiredFor childId)
                tampered = childIssue{githubIssueBody = "tampered"}
                labels = Set.toAscList (Set.union (desiredLabels (desiredFor parentId)) (desiredLabels (desiredFor childId)))
            journal <- newIORef []
            let client = scriptedClient $ \_ args _ -> do
                    modifyIORef' journal (<> [args])
                    pure (parentWriterResponse [parentIssue, childIssue] labels tampered args)
            result <- try (reconcile (Set.fromList [parentId, childId]) client spec fixtureTarget snapshot)
            failureCodeOf result `shouldBe` Just 1
            calls <- readIORef journal
            filter (any (isInfixOfArg "/sub_issues")) calls `shouldBe` []
        it "stops before mutation when the source ref moves" $ withWriterSnapshot $ \spec snapshot -> do
            journal <- newIORef []
            moved <- newIORef False
            let client = statefulClient journal $ \args -> do
                    alreadyMoved <- readIORef moved
                    if "graphql" `elem` args && not alreadyMoved
                        then do
                            writeIORef moved True
                            moveMainRef (sourceRoot spec)
                            pure WriterEmpty
                        else pure WriterEmpty
            value <- fixtureUuid
            result <- try (reconcile (Set.singleton value) client spec fixtureTarget snapshot)
            failureCodeOf result `shouldBe` Just 3
            calls <- readIORef journal
            filter isWriteCall calls `shouldBe` []
        it "stops before mutation when repository identity changes" $ withWriterSnapshot $ \spec snapshot -> do
            journal <- newIORef []
            repositoryReads <- newIORef (0 :: Int)
            let client = statefulClient journal $ \args -> do
                    if isRepositoryGet args
                        then do
                            count <- readIORef repositoryReads
                            writeIORef repositoryReads (count + 1)
                            pure (WriterRepositoryId (if count == 0 then 1 else 2))
                        else pure WriterEmpty
            value <- fixtureUuid
            result <- try (reconcile (Set.singleton value) client spec fixtureTarget snapshot)
            failureCodeOf result `shouldBe` Just 3
            calls <- readIORef journal
            filter isWriteCall calls `shouldBe` []
    describe "GitHub discovery" $ do
        it "links N:M PRs, warns on unknown and malformed links, and keeps deleted heads" $ do
            snapshot <- fixtureSnapshot
            first <- fixtureUuid
            second <- otherUuid
            unknown <- thirdUuid
            let validBody = trailer [first, second]
                secondBody = trailer [first]
                unknownBody = trailer [unknown]
                malformedBody = "<!-- myque:pr-links=github/v1 -->\nnope\n<!-- myque:pr-links:end -->\n"
                client =
                    discoveryClient
                        [corePr 1 "OPEN" False Nothing validBody, corePr 2 "MERGED" False (Just "fork/repo") secondBody, corePr 3 "CLOSED" False (Just "fork/repo") unknownBody, corePr 4 "OPEN" False (Just "fork/repo") malformedBody]
                        (factsResponse [(1, "sha", Just "SUCCESS", Just "APPROVED"), (2, "sha", Nothing, Nothing)])
            github <- discoverGithub client fixtureTarget snapshot
            map linkedPrNumber (githubPrsByUuid github Map.! first) `shouldBe` [1, 2]
            map linkedPrNumber (githubPrsByUuid github Map.! second) `shouldBe` [1]
            map linkedPrHeadRepo (githubPrsByUuid github Map.! first) `shouldContain` [Nothing]
            map warningCode (githubWarnings github) `shouldMatchList` ["unknown-pr-link", "malformed-pr-links"]
        it "marks optional facts unknown when the observed head SHA changed" $ do
            snapshot <- fixtureSnapshot
            first <- fixtureUuid
            let client = discoveryClient [corePr 1 "OPEN" False (Just "fork/repo") (trailer [first])] (factsResponse [(1, "different", Just "SUCCESS", Just "APPROVED")])
            github <- discoverGithub client fixtureTarget snapshot
            map linkedPrCi (githubPrsByUuid github Map.! first) `shouldBe` [CiUnknown]
            map linkedPrReview (githubPrsByUuid github Map.! first) `shouldBe` [ReviewUnknown]
    describe "committed snapshots" $ do
        it "reads committed content instead of working-tree edits" $ withSystemTempDirectory "myque-gh-source" $ \root -> do
            initializeRepo root
            let itemPath = root </> ".tasks" </> "items" </> "019a10d8-8d48-7b77-a414-f95ab7af31be.md"
            writeFile itemPath itemOne
            git root ["add", "."]
            gitEnv root ["commit", "-m", "fixture"]
            writeFile itemPath (replaceTitle "working title" itemOne)
            withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) $ \snapshot -> do
                map show (storeItems (snapshotStore snapshot)) `shouldSatisfy` any (isInfixOf "committed title")
        it "classifies a missing items tree as exit 2" $ withSystemTempDirectory "myque-gh-source" $ \root -> do
            initializeRepo root
            writeFile (root </> ".tasks" </> "config.toml") "schema = \"tracker-config/v1\"\n"
            git root ["add", "."]
            gitEnv root ["commit", "-m", "fixture"]
            result <- try (withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) (const (pure ())))
            failureCodeOf result `shouldBe` Just 2

scriptedClient :: (FilePath -> [String] -> Maybe B8.ByteString -> IO CommandResult) -> GithubClient
scriptedClient = GithubClient "gh"

okIncluded :: Value -> CommandResult
okIncluded value = CommandResult ExitSuccess ("HTTP/2 200 OK\r\ncontent-type: application/json\r\n\r\n" <> BL.toStrict (encode value)) ""

failureCodeOf :: Either Failure a -> Maybe Int
failureCodeOf (Left failure) = Just (failureCode failure)
failureCodeOf (Right _) = Nothing

fixtureTarget :: Target
fixtureTarget = Target "owner" "repo" (Set.singleton "bot")

fixtureGithub :: Map.Map Uuid GithubIssue -> [GithubLabel] -> GithubSnapshot
fixtureGithub issues labels = GithubSnapshot (RepositoryMeta 1 "owner/repo" False True) (Map.elems issues) labels issues Map.empty []

fixtureIssue :: Int -> Uuid -> WorkItem -> GithubIssue
fixtureIssue number value item = GithubIssue (fromIntegral number) number ("node" <> T.pack (show number)) "bot" (renderIssueIdentity value <> itemBody item) (itemTitleText item) IssueOpen Nothing Set.empty "now" Nothing Nothing

itemTitleText :: WorkItem -> Text
itemTitleText item = case T.lines (itemBody item) of
    line : _ -> T.drop 2 line
    [] -> ""

fixtureSnapshot :: IO Snapshot
fixtureSnapshot = makeSnapshot []

fixtureSnapshotWithTags :: [Text] -> IO Snapshot
fixtureSnapshotWithTags = makeSnapshot
fixtureSnapshotWithMilestone :: IO Snapshot
fixtureSnapshotWithMilestone = makeKindSnapshot Task Milestone

fixtureSnapshotWithParentKinds :: IO Snapshot
fixtureSnapshotWithParentKinds = withSystemTempDirectory "myque-gh-parent-kind-fixture" $ \root -> do
    layout <- initLayout root
    created <- either fail pure (parseTimestamp "2026-08-20T06:21:00Z")
    parentId <- fixtureUuid
    childId <- otherUuid
    let parentItem = newWorkItem parentId Milestone created "roadmap container"
        childItem = (newWorkItem childId Task created "selected child"){itemParent = Just parentId}
    mapM_ (saveItem layout) [parentItem, childItem]
    git root ["init", "-q"]
    git root ["add", "."]
    gitEnv root ["commit", "-m", "fixture"]
    withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) pure

makeKindSnapshot :: Kind -> Kind -> IO Snapshot
makeKindSnapshot firstKind secondKind = withSystemTempDirectory "myque-gh-kind-fixture" $ \root -> do
    layout <- initLayout root
    created <- either fail pure (parseTimestamp "2026-08-20T06:21:00Z")
    first <- fixtureUuid
    second <- otherUuid
    mapM_ (saveItem layout) [newWorkItem first firstKind created "first item", newWorkItem second secondKind created "second item"]
    git root ["init", "-q"]
    git root ["add", "."]
    gitEnv root ["commit", "-m", "fixture"]
    withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) pure

selectionFor :: Text -> Snapshot -> IO (Set.Set Uuid)
selectionFor expression snapshot = do
    query <- either fail pure (parseQuery expression)
    either (fail . T.unpack) pure (selectProjection query snapshot)

defaultSelection :: Snapshot -> IO (Set.Set Uuid)
defaultSelection = selectionFor defaultProjectionQuery

blankIssue :: Int -> Uuid -> GithubIssue
blankIssue number value = GithubIssue (fromIntegral number) number ("node" <> T.pack (show number)) "bot" (renderIssueIdentity value) "" IssueOpen Nothing Set.empty "now" Nothing Nothing

snapshotWithoutParent :: Snapshot -> Uuid -> Snapshot
snapshotWithoutParent snapshot uuid = snapshot{snapshotStore = store{storeById = Map.adjust (\item -> item{itemParent = Nothing}) uuid (storeById store)}}
  where
    store = snapshotStore snapshot

makeSnapshot :: [Text] -> IO Snapshot
makeSnapshot tags = withSystemTempDirectory "myque-gh-snapshot-fixture" $ \root -> do
    layout <- initLayout root
    created <- either fail pure (parseTimestamp "2026-08-20T06:21:00Z")
    closed <- either fail pure (parseTimestamp "2026-08-26T06:21:00Z")
    first <- fixtureUuid
    second <- otherUuid
    let openItem = (newWorkItem first Task created "open item"){itemTags = tags}
        doneItem = (newWorkItem second Task created "done item"){itemState = Done, itemClosed = Just closed}
    mapM_ (saveItem layout) [openItem, doneItem]
    git root ["init", "-q"]
    git root ["add", "."]
    gitEnv root ["commit", "-m", "fixture"]
    withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) pure

data WriterView
    = WriterEmpty
    | WriterStable GithubIssue
    | WriterRepositoryId Integer

statefulClient :: IORef [[String]] -> ([String] -> IO WriterView) -> GithubClient
statefulClient journal viewFor = scriptedClient $ \_ args _ -> do
    modifyIORef' journal (<> [args])
    view <- viewFor args
    pure (writerResponse view args)

writerResponse :: WriterView -> [String] -> CommandResult
writerResponse view args
    | isRepositoryGet args = okIncluded (repositoryWithId repositoryIdentity)
    | any (isInfixOfArg "/issues?state=all") args = jsonResult (Array (maybe mempty (pure . issueValue) visibleIssue))
    | any (isInfixOfArg "/labels?") args = jsonResult (Array (foldMap (foldMap (pure . labelValue) . Set.toAscList . githubIssueLabels) visibleIssue))
    | "graphql" `elem` args = jsonResult emptyPullRequests
    | any (isInfixOfArg "/issues/7") args = maybe unexpected (okIncluded . issueValue) visibleIssue
    | otherwise = unexpected
  where
    repositoryIdentity = case view of
        WriterRepositoryId value -> value
        _ -> 1
    visibleIssue = case view of
        WriterStable issue -> Just issue
        _ -> Nothing
    unexpected = CommandResult (ExitFailure 1) "" "unexpected writer test call"

repositoryWithId :: Integer -> Value
repositoryWithId value = object ["id" .= value, "full_name" .= ("owner/repo" :: Text), "archived" .= False, "has_issues" .= True]

emptyPullRequests :: Value
emptyPullRequests = object ["data" .= object ["repository" .= object ["pullRequests" .= object ["nodes" .= ([] :: [Value]), "pageInfo" .= object ["hasNextPage" .= False, "endCursor" .= Null]]]]]

issueValue :: GithubIssue -> Value
issueValue issue =
    object
        [ "id" .= githubIssueId issue
        , "number" .= githubIssueNumber issue
        , "node_id" .= githubIssueNodeId issue
        , "user" .= object ["login" .= githubIssueAuthor issue]
        , "body" .= githubIssueBody issue
        , "title" .= githubIssueTitle issue
        , "state" .= issueStateValue (githubIssueState issue)
        , "state_reason" .= fmap closeReasonValue (githubIssueReason issue)
        , "labels" .= map (\name -> object ["name" .= name]) (Set.toAscList (githubIssueLabels issue))
        , "updated_at" .= githubIssueUpdatedAt issue
        , "html_url" .= githubIssueUrl issue
        , "parent_issue_url" .= fmap issueRefUrl (githubIssueParent issue)
        ]

issueRefUrl :: GithubIssueRef -> Text
issueRefUrl reference = "https://api.github.com/repos/" <> githubIssueRefOwner reference <> "/" <> githubIssueRefRepo reference <> "/issues/" <> T.pack (show (githubIssueRefNumber reference))

issueStateValue :: IssueState -> Text
issueStateValue IssueOpen = "open"
issueStateValue IssueClosed = "closed"

closeReasonValue :: CloseReason -> Text
closeReasonValue Completed = "completed"
closeReasonValue NotPlanned = "not_planned"

labelValue :: Text -> Value
labelValue name = object ["name" .= name, "color" .= ("ededed" :: Text), "description" .= ("Managed by myque-gh." :: Text)]

issueFromDesired :: Int -> DesiredIssue -> GithubIssue
issueFromDesired number desired =
    GithubIssue
        (fromIntegral number)
        number
        ("node" <> T.pack (show number))
        "bot"
        (desiredBody desired)
        (desiredTitle desired)
        (desiredState desired)
        (desiredReason desired)
        (desiredLabels desired)
        "now"
        (Just ("https://github.com/owner/repo/issues/" <> T.pack (show number)))
        Nothing

withWriterSnapshot :: (SourceSpec -> Snapshot -> IO a) -> IO a
withWriterSnapshot action = withSystemTempDirectory "myque-gh-writer" $ \root ->
    withEnvironment "XDG_CACHE_HOME" (root </> "cache") $ do
        layout <- initLayout root
        created <- either fail pure (parseTimestamp "2026-08-20T06:21:00Z")
        value <- fixtureUuid
        _ <- saveItem layout (newWorkItem value Task created "open item")
        git root ["init", "-q", "--initial-branch=main"]
        git root ["add", "."]
        gitEnv root ["commit", "-m", "fixture"]
        let spec = SourceSpec root "refs/heads/main" Nothing Nothing Nothing
        withSnapshot spec (action spec)

withParentWriterSnapshot :: (SourceSpec -> Snapshot -> IO a) -> IO a
withParentWriterSnapshot action = withSystemTempDirectory "myque-gh-parent-writer" $ \root ->
    withEnvironment "XDG_CACHE_HOME" (root </> "cache") $ do
        layout <- initLayout root
        created <- either fail pure (parseTimestamp "2026-08-20T06:21:00Z")
        parentId <- fixtureUuid
        childId <- otherUuid
        let parentItem = newWorkItem parentId Task created "parent item"
            childItem = (newWorkItem childId Task created "child item"){itemParent = Just parentId}
        mapM_ (saveItem layout) [parentItem, childItem]
        git root ["init", "-q", "--initial-branch=main"]
        git root ["add", "."]
        gitEnv root ["commit", "-m", "fixture"]
        let spec = SourceSpec root "refs/heads/main" Nothing Nothing Nothing
        withSnapshot spec (action spec)

laggingResponse :: GithubIssue -> [String] -> CommandResult
laggingResponse issue args
    | isRepositoryGet args = okIncluded repositoryValue
    | any (isInfixOfArg "/issues?state=all") args = jsonResult (Array mempty)
    | any (isInfixOfArg "/labels?") args = jsonResult (Array mempty)
    | "graphql" `elem` args = jsonResult emptyPullRequests
    | isWriteCall args && any (isInfixOfArg "/labels") args = okIncluded Null
    | isWriteCall args && any (isInfixOfArg "/issues") args = okIncluded (issueValue issue)
    | any (isInfixOfArg "/issues/7") args = okIncluded (issueValue issue)
    | otherwise = CommandResult (ExitFailure 1) "" "unexpected lagging writer call"

parentWriterResponse :: [GithubIssue] -> [Text] -> GithubIssue -> [String] -> CommandResult
parentWriterResponse issues labels tamperedChild args
    | isRepositoryGet args = okIncluded repositoryValue
    | any (isInfixOfArg "/issues?state=all") args = jsonResult (Array (foldMap (pure . issueValue) issues))
    | any (isInfixOfArg "/labels?") args = jsonResult (Array (foldMap (pure . labelValue) labels))
    | "graphql" `elem` args = jsonResult emptyPullRequests
    | any (isInfixOfArg "/issues/8") args = okIncluded (issueValue tamperedChild)
    | any (isInfixOfArg "/issues/7") args = maybe unexpected (okIncluded . issueValue) (lookupNumber 7)
    | otherwise = unexpected
  where
    lookupNumber number = case filter ((== number) . githubIssueNumber) issues of
        issue : _ -> Just issue
        [] -> Nothing
    unexpected = CommandResult (ExitFailure 1) "" "unexpected parent writer call"

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value = bracket (lookupEnv name <* setEnv name value) restore . const
  where
    restore (Just previous) = setEnv name previous
    restore Nothing = unsetEnv name

moveMainRef :: FilePath -> IO ()
moveMainRef root = do
    writeFile (root </> "moved.txt") "moved\n"
    git root ["add", "moved.txt"]
    gitEnv root ["commit", "-m", "move source ref"]

isRepositoryGet :: [String] -> Bool
isRepositoryGet args = "/repos/owner/repo" `elem` args && argumentValue "--method" args == Just "GET"

isWriteCall :: [String] -> Bool
isWriteCall args = maybe False (`elem` ["POST", "PATCH", "DELETE"]) (argumentValue "--method" args)

argumentValue :: String -> [String] -> Maybe String
argumentValue name args = case dropWhile (/= name) args of
    _ : value : _ -> Just value
    _ -> Nothing

isInfixOfArg :: String -> String -> Bool
isInfixOfArg = isInfixOf

repositoryValue :: Value
repositoryValue = object ["id" .= (1 :: Int), "full_name" .= ("owner/repo" :: Text), "archived" .= False, "has_issues" .= True]

discoveryClient :: [Value] -> Value -> GithubClient
discoveryClient prs facts = scriptedClient $ \_ args _ -> pure $ case args of
    _ | "--include" `elem` args && "/repos/owner/repo" `elem` args -> okIncluded repositoryValue
    _ | any (isInfixOfArg "/issues?") args -> jsonResult (Array mempty)
    _ | any (isInfixOfArg "/labels?") args -> jsonResult (Array mempty)
    _ | "graphql" `elem` args && any (isInfixOfArg "statusCheckRollup") args -> jsonResult facts
    _ | "graphql" `elem` args -> jsonResult (object ["data" .= object ["repository" .= object ["pullRequests" .= object ["nodes" .= prs, "pageInfo" .= object ["hasNextPage" .= False, "endCursor" .= Null]]]]])
    _ -> CommandResult (ExitFailure 1) "" "unexpected fake gh call"

jsonResult :: Value -> CommandResult
jsonResult value = CommandResult ExitSuccess (BL.toStrict (encode [value])) ""

corePr :: Int -> Text -> Bool -> Maybe Text -> Text -> Value
corePr number state draft headRepo body =
    object
        [ "number" .= number
        , "title" .= ("PR" :: Text)
        , "body" .= body
        , "state" .= state
        , "isDraft" .= draft
        , "headRefName" .= ("feature" :: Text)
        , "headRefOid" .= ("sha" :: Text)
        , "baseRefName" .= ("main" :: Text)
        , "baseRefOid" .= ("base" :: Text)
        , "headRepository" .= fmap (\name -> object ["nameWithOwner" .= name]) headRepo
        , "baseRepository" .= object ["nameWithOwner" .= ("owner/repo" :: Text)]
        , "updatedAt" .= ("now" :: Text)
        ]

factsResponse :: [(Int, Text, Maybe Text, Maybe Text)] -> Value
factsResponse values = object ["data" .= object ["repository" .= object [(Key.fromText (T.pack ("p" <> show number)), object ["headRefOid" .= sha, "statusCheckRollup" .= fmap (\state -> object ["state" .= state]) ci, "reviewDecision" .= review]) | (number, sha, ci, review) <- values]]]

trailer :: [Uuid] -> Text
trailer values = T.unlines (["<!-- myque:pr-links=github/v1 -->"] <> map (\value -> "<!-- myque:implements=" <> T.pack (show value) <> " -->") values <> ["<!-- myque:pr-links:end -->"])

renderTrailerFor :: Uuid -> Text
renderTrailerFor value = trailer [value]

requireRight :: (Show e) => Either e a -> IO a
requireRight (Right value) = pure value
requireRight (Left err) = expectationFailure (show err) >> fail "unreachable"

fixtureUuid :: IO Uuid
fixtureUuid = either fail pure (parseUuid "019a10d8-8d48-7b77-a414-f95ab7af31be")

otherUuid :: IO Uuid
otherUuid = either fail pure (parseUuid "019a10d8-8d48-7b77-a414-f95ab7af31bf")

thirdUuid :: IO Uuid
thirdUuid = either fail pure (parseUuid "019a10d8-8d48-7b77-a414-f95ab7af31c0")

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

initializeRepo :: FilePath -> IO ()
initializeRepo root = do
    createDirectoryIfMissing True (root </> ".tasks" </> "items")
    git root ["init", "-q"]
    writeFile (root </> ".tasks" </> "config.toml") "schema = \"tracker-config/v1\"\n\n[storage]\nitems = \".tasks/items\"\n"

git :: FilePath -> [String] -> IO ()
git root args = callProcess "git" (["-C", root] <> args)

gitEnv :: FilePath -> [String] -> IO ()
gitEnv root args = callProcess "git" (["-C", root, "-c", "user.name=Fixture", "-c", "user.email=fixture@example.test", "-c", "commit.gpgsign=false"] <> args)

itemOne :: String
itemOne = "---\nschema: work-item/v1\nid: 019a10d8-8d48-7b77-a414-f95ab7af31be\nkind: task\nstate: open\ncreated: 2026-08-20T06:21:00Z\n---\n\n# committed title\n"

replaceTitle :: String -> String -> String
replaceTitle title = unlines . map (\line -> if line == "# committed title" then "# " <> title else line) . lines
