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
import Myque.Github.Github (discoverGithub, getRepository, graphqlPaginated, restSingle)
import Myque.Github.Markers
import Myque.Github.Projection (ProjectionContext (..), projectIssue, renderDisplay)
import Myque.Github.Reconcile (applyParentChange, buildPlan, diffIssue, planIsEmpty, reconcile)
import Myque.Github.Source (decodeBlobBatch, withSnapshot)
import Myque.Github.Types
import Myque.Item (Kind (..), State (..), WorkItem (..), newWorkItem)
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
        it "ignores fenced samples and rejects malformed or multiple trailers" $ do
            value <- fixtureUuid
            let marker = "<!-- myque:implements=" <> T.pack (show value) <> " -->"
                sample = "```md\n<!-- myque:pr-links=github/v1 -->\n" <> marker <> "\n<!-- myque:pr-links:end -->\n```\n"
                malformed = "body\n<!-- myque:pr-links=github/v1 -->\n" <> marker <> "\n"
                duplicate = T.concat [renderTrailerFor value, "\n", renderTrailerFor value]
            parsedPrUuids (parsePrLinks sample) `shouldBe` Set.empty
            rewritePrLinks malformed (Set.singleton value) `shouldSatisfy` isLeft
            rewritePrLinks duplicate Set.empty `shouldSatisfy` isLeft
    describe "issue drift" $ do
        it "preserves human labels while replacing managed labels" $ do
            value <- fixtureUuid
            let desired = DesiredIssue value "A" "title" "body" IssueOpen Nothing (Set.fromList ["myque:state:open"])
                actual = GithubIssue 1 1 "N" "bot" "body" "title" IssueOpen Nothing (Set.fromList ["human", "myque:state:done"]) "now" Nothing Nothing
            diffIssue desired actual `shouldBe` [RemoveLabel "myque:state:done", AddLabel "myque:state:open"]
    describe "projection planning" $ do
        it "creates only first-seen non-terminal items and keeps managed terminal items" $ do
            snapshot <- fixtureSnapshot
            openId <- fixtureUuid
            doneId <- otherUuid
            let openItem = storeById (snapshotStore snapshot) Map.! openId
                doneItem = storeById (snapshotStore snapshot) Map.! doneId
                target = fixtureTarget
                emptyRemote = fixtureGithub Map.empty []
            plan <- requireRight (buildPlan target snapshot emptyRemote)
            Map.keys (planIssueCreates plan) `shouldBe` [openId]
            let existing = fixtureIssue 9 doneId doneItem
            terminalPlan <- requireRight (buildPlan target snapshot (fixtureGithub (Map.singleton doneId existing) []))
            any (\(uuid, _, _) -> uuid == doneId) (Map.elems (planIssueChanges terminalPlan)) `shouldBe` True
            itemState openItem `shouldBe` Open
        it "rejects case-colliding managed tag labels before mutation" $ do
            snapshot <- fixtureSnapshotWithTags ["Foo", "foo"]
            buildPlan fixtureTarget snapshot (fixtureGithub Map.empty []) `shouldSatisfy` isLeft
        it "creates terminal ancestors and plans native parent changes" $ do
            snapshot <- fixtureSnapshotWithParent
            parentId <- fixtureUuid
            childId <- otherUuid
            let parentItem = storeById (snapshotStore snapshot) Map.! parentId
                childItem = storeById (snapshotStore snapshot) Map.! childId
                parentIssue = fixtureIssue 7 parentId parentItem
                childIssue = fixtureIssue 8 childId childItem
                issues = Map.fromList [(parentId, parentIssue), (childId, childIssue)]
            createPlan <- requireRight (buildPlan fixtureTarget snapshot (fixtureGithub Map.empty []))
            Map.keys (planIssueCreates createPlan) `shouldMatchList` [parentId, childId]
            planParentChanges createPlan `shouldBe` Map.singleton childId (renderDisplay snapshot childItem, Just parentId)
            linkPlan <- requireRight (buildPlan fixtureTarget snapshot (fixtureGithub issues []))
            planParentChanges linkPlan `shouldBe` Map.singleton childId (renderDisplay snapshot childItem, Just parentId)
            childOnly <- requireRight (buildPlan fixtureTarget snapshot (fixtureGithub (Map.singleton childId childIssue) []))
            planParentChanges childOnly `shouldBe` Map.singleton childId (renderDisplay snapshot childItem, Just parentId)
            let linkedChild = childIssue{githubIssueParent = Just (GithubIssueRef "owner" "repo" 7)}
            converged <- requireRight (buildPlan fixtureTarget snapshot (fixtureGithub (Map.insert childId linkedChild issues) []))
            planParentChanges converged `shouldBe` Map.empty
        it "plans removal and replacement of existing parents" $ do
            snapshot <- fixtureSnapshotWithParent
            parentId <- fixtureUuid
            childId <- otherUuid
            let parentItem = storeById (snapshotStore snapshot) Map.! parentId
                childItem = storeById (snapshotStore snapshot) Map.! childId
                parentIssue = fixtureIssue 7 parentId parentItem
                childIssue = (fixtureIssue 8 childId childItem){githubIssueParent = Just (GithubIssueRef "owner" "repo" 99)}
                issues = Map.fromList [(parentId, parentIssue), (childId, childIssue)]
            replacePlan <- requireRight (buildPlan fixtureTarget snapshot (fixtureGithub issues []))
            Map.lookup childId (planParentChanges replacePlan) `shouldBe` Just (renderDisplay snapshot childItem, Just parentId)
            let detachedSnapshot = snapshotWithoutParent snapshot childId
            removePlan <- requireRight (buildPlan fixtureTarget detachedSnapshot (fixtureGithub issues []))
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
    describe "writer transactions" $ do
        it "rediscovers an existing projection without duplicate writes" $ withWriterSnapshot $ \spec snapshot -> do
            value <- fixtureUuid
            let item = storeById (snapshotStore snapshot) Map.! value
                desired = projectIssue (ProjectionContext snapshot fixtureTarget (Map.singleton value 7)) item []
                issue = issueFromDesired 7 desired
            journal <- newIORef []
            let client = statefulClient journal (const (pure (WriterStable issue)))
            plan <- reconcile client spec fixtureTarget snapshot
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
            plan <- reconcile client spec fixtureTarget snapshot
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
            result <- try (reconcile client spec fixtureTarget snapshot)
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
            result <- try (reconcile client spec fixtureTarget snapshot)
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
            result <- try (reconcile client spec fixtureTarget snapshot)
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

fixtureSnapshotWithParent :: IO Snapshot
fixtureSnapshotWithParent = withSystemTempDirectory "myque-gh-parent-fixture" $ \root -> do
    layout <- initLayout root
    created <- either fail pure (parseTimestamp "2026-08-20T06:21:00Z")
    parentId <- fixtureUuid
    childId <- otherUuid
    let parentItem = (newWorkItem parentId Task created "terminal parent"){itemState = Done, itemClosed = Just created}
        childItem = (newWorkItem childId Task created "open child"){itemParent = Just parentId}

    mapM_ (saveItem layout) [parentItem, childItem]
    git root ["init", "-q"]
    git root ["add", "."]
    gitEnv root ["commit", "-m", "fixture"]
    withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) pure

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
