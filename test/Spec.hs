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
import Myque.Github.Body (BodyRenderer (..), prepareBodies, validateProjectionBodies)
import Myque.Github.Cli (runCli)
import Myque.Github.Github (discoverGithub, encodePathSegment, getRepository, graphqlPaginated, restSingle)
import Myque.Github.Markers
import Myque.Github.Projection (ProjectionContext (..), projectIssue, projectMilestone, renderDisplay)
import Myque.Github.Reconcile (applyIssueDiff, applyParentChange, buildPlan, defaultProjectionQuery, diffIssue, planIsEmpty, reconcile, selectProjection)
import Myque.Github.Source (decodeBlobBatch, withSnapshot)
import Myque.Github.Types
import Myque.Graph (edgesOf, isReady)
import Myque.Item (Kind (..), State (..), WorkItem (..), newWorkItem)
import Myque.Query (parseQuery)
import Myque.Store (History (..), Store (..), TerminalRecord (..), initLayout, saveItem, storeItems)
import Myque.Terminal (encodeTerminal)
import Myque.Timestamp (parseTimestamp)
import Myque.Uuid (Uuid, parseUuid, uuidText)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "consumer Markdown rendering" $ do
        it "preserves legacy Markdown byte-for-byte without a renderer" $ do
            original <- fixtureSnapshot
            uuid <- fixtureUuid
            let prose = "# open item\n\nLegacy prose.\r\n\n```zt\nvalue ::= 64;\nvalue\n```\n\n````markdown\n```zti\n{ example = true; }\n```\n````\n"
                snapshot = adjustSnapshotItem uuid (\item -> item{itemBody = prose}) original
            prepared <- prepareBodies Nothing snapshot
            let item = storeById (snapshotStore prepared) Map.! uuid
                desired = projectIssue (ProjectionContext prepared fixtureTarget Map.empty) item []
            desiredBody desired `shouldSatisfy` T.isSuffixOf prose
            desiredUuid desired `shouldBe` uuid
        it "renders human Markdown while retaining UUID markers and PR identity" $ do
            original <- fixtureSnapshot
            uuid <- fixtureUuid
            let snapshot = adjustSnapshotItem uuid (\item -> item{itemBody = "# open item\n\n```zti\n{ problem = \"bounded queue\"; }\n```\n"}) original
                readable = "## Problem\nBounded queue\n\n## Acceptance\n- A1: Reject overload.\n"
            prepared <- prepareBodies (Just (BodyRenderer "printf" [T.unpack readable])) snapshot
            let item = storeById (snapshotStore prepared) Map.! uuid
                desired = projectIssue (ProjectionContext prepared fixtureTarget Map.empty) item []
            desiredBody desired `shouldSatisfy` T.isSuffixOf readable
            desiredBody desired `shouldSatisfy` (not . T.isInfixOf "```zti")
            parseIssueIdentity (desiredBody desired) `shouldBe` Right (Just uuid)
            parsedPrUuids (parsePrLinks (trailer [uuid])) `shouldBe` Set.singleton uuid
        it "refuses unrendered structured fences and a renderer that returns raw data" $ do
            original <- fixtureSnapshot
            uuid <- fixtureUuid
            let raw = "```zti\n{ problem = \"queue\"; }\n```\n"
                snapshot = adjustSnapshotItem uuid (\item -> item{itemBody = raw}) original
            validateProjectionBodies snapshot `shouldSatisfy` isLeft
            unconfigured <- try (prepareBodies Nothing snapshot)
            failureCodeOf unconfigured `shouldBe` Just 1
            unsafe <- try (prepareBodies (Just (BodyRenderer "printf" [T.unpack raw])) snapshot)
            failureCodeOf unsafe `shouldBe` Just 1
            buildPlan Set.empty fixtureTarget snapshot (fixtureGithub Map.empty []) `shouldSatisfy` isLeft
        it "refuses every structured body the projection would actually emit" $ do
            original <- fixtureSnapshot
            uuid <- fixtureUuid
            let bodies =
                    [ "```zt\nvalue ::= 64;\n```\n"
                    , "```zt\nvalue ::= 64;\n```\n\n# Notes\n\nprose\n"
                    , "# open item\n\n```zt profile=requirements\nvalue ::= 64;\n```\n"
                    , "# open item\n\n~~~zti lang=x\n{ problem = \"queue\"; }\n~~~\n"
                    ]
            mapM_
                ( \body -> do
                    let snapshot = adjustSnapshotItem uuid (\item -> item{itemBody = body}) original
                    validateProjectionBodies snapshot `shouldSatisfy` isLeft
                    unconfigured <- try (prepareBodies Nothing snapshot)
                    failureCodeOf unconfigured `shouldBe` Just 1
                    buildPlan Set.empty fixtureTarget snapshot (fixtureGithub Map.empty []) `shouldSatisfy` isLeft
                )
                bodies
        it "keeps a heading-less description instead of silently dropping it" $ do
            original <- fixtureSnapshot
            uuid <- fixtureUuid
            let prose = "Required description without any heading.\n"
                snapshot = adjustSnapshotItem uuid (\item -> item{itemBody = prose}) original
            prepared <- prepareBodies Nothing snapshot
            let item = storeById (snapshotStore prepared) Map.! uuid
            desiredBody (projectIssue (ProjectionContext prepared fixtureTarget Map.empty) item []) `shouldSatisfy` T.isSuffixOf prose
            empty <- try (prepareBodies (Just (BodyRenderer "printf" [""])) snapshot)
            failureCodeOf empty `shouldBe` Just 1
        it "refuses failed, missing, empty and non-UTF8 renderers without fallback" $ do
            original <- fixtureSnapshot
            uuid <- fixtureUuid
            let snapshot = adjustSnapshotItem uuid (\item -> item{itemBody = "# item\n\nRequired description\n"}) original
            mapM_
                ( \renderer -> do
                    result <- try (prepareBodies (Just renderer) snapshot)
                    failureCodeOf result `shouldBe` Just 1
                )
                [ BodyRenderer "false" []
                , BodyRenderer "/nonexistent/myque-gh-renderer" []
                , BodyRenderer "printf" [""]
                , BodyRenderer "printf" ["\\377"]
                , BodyRenderer "printf" ["<!-- myque:id=spoof -->"]
                ]
        it "passes literal argument vectors rather than a shell command" $ do
            snapshot <- fixtureSnapshot
            uuid <- fixtureUuid
            let literal = "$(exit 73); `exit 72`"
            prepared <- prepareBodies (Just (BodyRenderer "printf" ["%s", literal])) snapshot
            Map.lookup uuid (snapshotRenderedBodies prepared) `shouldBe` Just (T.pack literal)
    describe "retired identity projection" $ do
        it "resolves retired dependencies offline with done-only readiness" $ do
            snapshot <- fixtureSnapshot
            dependentId <- fixtureUuid
            dependencyId <- otherUuid
            let original = snapshotStore snapshot
                dependent = (storeById original Map.! dependentId){itemDepends = [dependencyId]}
                dependency = storeById original Map.! dependencyId
                history = History (T.replicate 40 "c") (T.replicate 40 "a") (canonicalRetiredPath dependencyId) (T.replicate 64 "b")
            mapM_
                ( \(state, ready) -> do
                    let terminalItemValue = dependency{itemState = state}
                        store = original{storeById = Map.fromList [(dependentId, dependent), (dependencyId, terminalItemValue)], storeTerminals = Map.singleton dependencyId (TerminalRecord terminalItemValue history "reason" (canonicalRetiredPath dependencyId))}
                    isReady store (edgesOf store) dependent `shouldBe` ready
                )
                [(Done, True), (Cancelled, False)]
        it "keeps done/cancelled reasons, immutable recovery links and reopening identity" $ do
            original <- fixtureSnapshot
            uuid <- fixtureUuid
            let active = storeById (snapshotStore original) Map.! uuid
                history = History (snapshotRepository original) (T.replicate 40 "a") (canonicalRetiredPath uuid) (T.replicate 64 "b")
                source = (snapshotSource original){sourceLinkRepo = Just ("owner", "repo"), sourceLinkBranch = Just "main"}
            mapM_
                ( \(state, reason) -> do
                    let item = active{itemState = state}
                        store = snapshotStore original
                        terminal = TerminalRecord item history "Verified closure or cancellation" (canonicalRetiredPath uuid)
                        retired = original{snapshotSource = source, snapshotStore = store{storeById = Map.insert uuid item (storeById store), storeTerminals = Map.singleton uuid terminal}}
                        desired = projectIssue (ProjectionContext retired fixtureTarget Map.empty) item []
                    desiredState desired `shouldBe` IssueClosed
                    desiredReason desired `shouldBe` Just reason
                    parseIssueIdentity (desiredBody desired) `shouldBe` Right (Just uuid)
                    desiredBody desired `shouldSatisfy` T.isInfixOf ("/blob/" <> T.replicate 40 "a" <> "/" <> canonicalRetiredPath uuid)
                    desiredBody desired `shouldSatisfy` T.isInfixOf "not available offline"
                    let reopened = projectIssue (ProjectionContext original fixtureTarget Map.empty) active []
                    desiredUuid reopened `shouldBe` desiredUuid desired
                    desiredState reopened `shouldBe` IssueOpen
                    desiredReason reopened `shouldBe` Nothing
                )
                [(Done, Completed), (Cancelled, NotPlanned)]
        it "never links retained history that belongs to another repository" $ do
            original <- fixtureSnapshot
            uuid <- fixtureUuid
            let active = storeById (snapshotStore original) Map.! uuid
                item = active{itemState = Done}
                store = snapshotStore original
                foreign' = History (T.replicate 40 "c") (T.replicate 40 "a") (canonicalRetiredPath uuid) (T.replicate 64 "b")
                source = (snapshotSource original){sourceLinkRepo = Just ("owner", "repo"), sourceLinkBranch = Just "main"}
                retired = original{snapshotSource = source, snapshotStore = store{storeById = Map.insert uuid item (storeById store), storeTerminals = Map.singleton uuid (TerminalRecord item foreign' "Verified closure" (canonicalRetiredPath uuid))}}
                desired = projectIssue (ProjectionContext retired fixtureTarget Map.empty) item []
            desiredBody desired `shouldSatisfy` (not . T.isInfixOf "/blob/")
            desiredBody desired `shouldSatisfy` T.isInfixOf (T.replicate 40 "a" <> ":" <> canonicalRetiredPath uuid)
            case buildPlan Set.empty fixtureTarget retired (fixtureGithub Map.empty []) of
                Left conflicts -> expectationFailure ("unexpected conflicts: " <> show conflicts)
                Right plan -> map warningCode (planWarnings plan) `shouldBe` ["foreign-retained-history"]
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
                actual = GithubIssue 1 1 "N" "bot" "body" "title" IssueOpen Nothing (Set.fromList ["human", "myque:state:done"]) "now" Nothing Nothing Nothing
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
        it "retains historical trusted projections excluded by the query" $ do
            snapshot <- fixtureSnapshot
            value <- fixtureUuid
            selected <- selectionFor "kind = bug" snapshot
            let item = storeById (snapshotStore snapshot) Map.! value
                historicalIssue = (fixtureIssue 8 value item){githubIssueAuthor = "historical-author", githubIssueTitle = "stale title"}
                migrationTarget = fixtureTarget{targetIssueAuthors = Set.fromList ["historical-author", "automation-bot"]}
                automationOnly = fixtureTarget{targetIssueAuthors = Set.singleton "automation-bot"}
            trustedGithub <- discoverGithub (discoveryClientWithIssues [historicalIssue] [] emptyPullRequests) migrationTarget snapshot
            trustedPlan <- requireRight (buildPlan selected migrationTarget snapshot trustedGithub)
            planIssueCreates trustedPlan `shouldBe` Map.empty
            Map.keys (planIssueChanges trustedPlan) `shouldBe` [8]
            untrustedGithub <- discoverGithub (discoveryClientWithIssues [historicalIssue] [] emptyPullRequests) automationOnly snapshot
            Map.member value (githubIssueByUuid untrustedGithub) `shouldBe` False
            githubWarnings untrustedGithub
                `shouldBe` [Warning "untrusted-identity-claim" ("issue #8 by historical-author claims " <> T.pack (show value))]
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
                actual = GithubIssue 1 1 "N" "bot" "body" "title" IssueOpen Nothing (Set.singleton "myque:tag:硬體") "now" Nothing Nothing Nothing
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
    describe "native milestone planning" $ do
        it "assigns through an epic without replacing the issue parent chain" $ do
            snapshot <- milestoneChain Epic
            outer <- fixtureUuid
            middle <- otherUuid
            child <- thirdUuid
            plan <- requireRight (buildPlan (Set.singleton child) fixtureTarget snapshot (fixtureGithub Map.empty []))
            Map.keys (planIssueCreates plan) `shouldMatchList` [outer, middle, child]
            Map.keys (planMilestoneCreates plan) `shouldBe` [outer]
            fmap snd (planParentChanges plan) `shouldBe` Map.fromList [(middle, Just outer), (child, Just middle)]
            fmap snd (planMilestoneAssignments plan) `shouldBe` Map.fromList [(middle, Just outer), (child, Just outer)]
        it "uses the nearest nested milestone and never assigns a container to itself" $ do
            snapshot <- milestoneChain Milestone
            outer <- fixtureUuid
            middle <- otherUuid
            child <- thirdUuid
            plan <- requireRight (buildPlan (Set.singleton child) fixtureTarget snapshot (fixtureGithub Map.empty []))
            Map.keys (planMilestoneCreates plan) `shouldMatchList` [outer, middle]
            fmap snd (planMilestoneAssignments plan) `shouldBe` Map.fromList [(middle, Just outer), (child, Just middle)]
        it "renames a retained UUID instead of creating another native milestone" $ do
            snapshot <- fixtureSnapshotWithParentKinds
            parent <- fixtureUuid
            let old = managedMilestone 41 parent "old title" IssueOpen
                github = withMilestones [(parent, old)] (fixtureGithub Map.empty [])
            plan <- requireRight (buildPlan Set.empty fixtureTarget snapshot github)
            planMilestoneCreates plan `shouldBe` Map.empty
            fmap desiredMilestoneTitle (Map.lookup parent (planMilestoneChanges plan)) `shouldBe` Just "roadmap container"
        it "closes retained done and cancelled native milestones" $ do
            snapshot <- fixtureSnapshotWithParentKinds
            parent <- fixtureUuid
            let old = managedMilestone 41 parent "roadmap container" IssueOpen
                github = withMilestones [(parent, old)] (fixtureGithub Map.empty [])
            mapM_
                ( \state -> do
                    let terminal = adjustSnapshotItem parent (\item -> item{itemState = state}) snapshot
                    plan <- requireRight (buildPlan Set.empty fixtureTarget terminal github)
                    fmap desiredMilestoneState (Map.lookup parent (planMilestoneChanges plan)) `shouldBe` Just IssueClosed
                )
                [Done, Cancelled]
        it "moves managed membership by UUID and clears it after detachment" $ do
            snapshot <- milestoneChain Milestone
            outer <- fixtureUuid
            middle <- otherUuid
            child <- thirdUuid
            let old = managedMilestone 41 outer "outer" IssueOpen
                nearest = managedMilestone 42 middle "middle" IssueOpen
                issue = (blankIssue 8 child){githubIssueMilestone = Just old}
                github = withMilestones [(outer, old), (middle, nearest)] (fixtureGithub (Map.singleton child issue) [])
            move <- requireRight (buildPlan (Set.singleton child) fixtureTarget snapshot github)
            fmap snd (Map.lookup child (planMilestoneAssignments move)) `shouldBe` Just (Just middle)
            clear <- requireRight (buildPlan (Set.singleton child) fixtureTarget (snapshotWithoutParent snapshot child) github)
            fmap snd (Map.lookup child (planMilestoneAssignments clear)) `shouldBe` Just Nothing
        it "leaves a kind-changed native orphan untouched while clearing its membership" $ do
            snapshot <- fixtureSnapshotWithParentKinds
            parent <- fixtureUuid
            child <- otherUuid
            let native = managedMilestone 41 parent "roadmap container" IssueOpen
                issue = (blankIssue 8 child){githubIssueMilestone = Just native}
                github = withMilestones [(parent, native)] (fixtureGithub (Map.singleton child issue) [])
                changed = adjustSnapshotItem parent (\item -> item{itemKind = Epic}) snapshot
            plan <- requireRight (buildPlan (Set.singleton child) fixtureTarget changed github)
            planMilestoneCreates plan `shouldBe` Map.empty
            planMilestoneChanges plan `shouldBe` Map.empty
            fmap snd (Map.lookup child (planMilestoneAssignments plan)) `shouldBe` Just Nothing
            planWarnings plan `shouldSatisfy` any (T.isInfixOf "41" . warningDetail)
        it "preserves manual membership unless a canonical milestone would replace it" $ do
            snapshot <- fixtureSnapshotWithParentKinds
            child <- otherUuid
            let manual = GithubMilestone 99 "human" "manual" "Human description" IssueOpen
                issue = (blankIssue 8 child){githubIssueMilestone = Just manual}
                github = (fixtureGithub (Map.singleton child issue) []){githubMilestones = [manual]}
            detached <- requireRight (buildPlan (Set.singleton child) fixtureTarget (snapshotWithoutParent snapshot child) github)
            Map.lookup child (planMilestoneAssignments detached) `shouldBe` Nothing
            buildPlan (Set.singleton child) fixtureTarget snapshot github `shouldSatisfy` isLeft
        it "refuses a native title collision rather than adopting an unrelated milestone" $ do
            snapshot <- fixtureSnapshotWithParentKinds
            child <- otherUuid
            let manual = GithubMilestone 99 "human" "roadmap container" "Human description" IssueOpen
                github = (fixtureGithub Map.empty []){githubMilestones = [manual]}
            buildPlan (Set.singleton child) fixtureTarget snapshot github `shouldSatisfy` isLeft
        it "refuses two canonical milestones with the same native title" $ do
            snapshot <- milestoneChain Milestone
            outer <- fixtureUuid
            middle <- otherUuid
            child <- thirdUuid
            let outerBody = itemBody (storeById (snapshotStore snapshot) Map.! outer)
                colliding = adjustSnapshotItem middle (\item -> item{itemBody = outerBody}) snapshot
            buildPlan (Set.singleton child) fixtureTarget colliding (fixtureGithub Map.empty []) `shouldSatisfy` isLeft
    describe "native milestone discovery" $ do
        it "discovers closed trusted identities and nested issue membership" $ do
            snapshot <- fixtureSnapshot
            value <- fixtureUuid
            let milestone = managedMilestone 41 value "released" IssueClosed
                issue = (blankIssue 8 value){githubIssueMilestone = Just milestone}
            github <- discoverGithub (discoveryClientWithMilestones [issue] [milestone] [] emptyPullRequests) fixtureTarget snapshot
            Map.lookup value (githubMilestoneByUuid github) `shouldBe` Just milestone
            fmap githubIssueMilestone (Map.lookup value (githubIssueByUuid github)) `shouldBe` Just (Just milestone)
        it "rejects duplicate trusted native identities" $ do
            snapshot <- fixtureSnapshot
            value <- fixtureUuid
            let first = managedMilestone 41 value "first" IssueOpen
                second = managedMilestone 42 value "second" IssueClosed
            result <- try (discoverGithub (discoveryClientWithMilestones [] [first, second] [] emptyPullRequests) fixtureTarget snapshot)
            result `shouldSatisfy` (isLeft :: Either Failure GithubSnapshot -> Bool)
        it "rejects incomplete and duplicate trusted milestone headers" $ do
            snapshot <- fixtureSnapshot
            value <- fixtureUuid
            let milestone = managedMilestone 41 value "release" IssueOpen
                headers = ["<!-- myque:id=" <> T.pack (show value) <> " -->\n", renderMilestoneIdentity value <> renderMilestoneIdentity value]
            mapM_
                ( \header -> do
                    result <- try (discoverGithub (discoveryClientWithMilestones [] [milestone{githubMilestoneDescription = header}] [] emptyPullRequests) fixtureTarget snapshot)
                    result `shouldSatisfy` (isLeft :: Either Failure GithubSnapshot -> Bool)
                )
                headers
        it "warns about untrusted claims without owning them or poisoning a trusted identity" $ do
            snapshot <- fixtureSnapshot
            value <- fixtureUuid
            let trusted = managedMilestone 41 value "release" IssueOpen
                impostor = trusted{githubMilestoneNumber = 42, githubMilestoneAuthor = "human"}
            github <- discoverGithub (discoveryClientWithMilestones [] [trusted, impostor] [] emptyPullRequests) fixtureTarget snapshot
            Map.lookup value (githubMilestoneByUuid github) `shouldBe` Just trusted
            githubWarnings github `shouldSatisfy` any (T.isInfixOf "42" . warningDetail)
        it "warns rather than failing on malformed untrusted claims" $ do
            snapshot <- fixtureSnapshot
            value <- fixtureUuid
            let impostor =
                    (managedMilestone 42 value "release" IssueOpen)
                        { githubMilestoneAuthor = "human"
                        , githubMilestoneDescription = "<!-- myque:id=" <> T.pack (show value) <> " -->\n"
                        }
            github <- discoverGithub (discoveryClientWithMilestones [] [impostor] [] emptyPullRequests) fixtureTarget snapshot
            githubMilestoneByUuid github `shouldBe` Map.empty
            githubWarnings github `shouldSatisfy` any (T.isInfixOf "42" . warningDetail)
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
        it "requires trusted authors for plan and apply before local or GitHub access" $ do
            planResult <- runCli ["plan", "--store", "/missing", "--repo", "owner/repo"]
            applyResult <- runCli ["apply", "--store", "/missing", "--repo", "owner/repo", "--ref", "refs/heads/main"]
            planResult `shouldBe` ExitFailure 2
            applyResult `shouldBe` ExitFailure 2
        it "keeps pr link independent of issue author configuration" $ do
            result <- runCli ["pr", "link", "0", "--store", "/missing", "--repo", "owner/repo", "--clear"]
            result `shouldBe` ExitFailure 2
        it "rejects an invalid project query before GitHub discovery" $ withSystemTempDirectory "myque-gh-invalid-query" $ \root -> do
            initializeRepo root
            writeFile (root </> ".tasks" </> "items" </> "019a10d8-8d48-7b77-a414-f95ab7af31be.md") itemOne
            git root ["add", "."]
            gitEnv root ["commit", "-m", "fixture"]
            result <- runCli ["plan", "--store", root, "--repo", "owner/repo", "--issue-author", "bot", "--project", "kind ="]
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
        it "preserves an intervening human milestone assignment" $ withParentWriterSnapshot $ \spec original -> do
            (snapshot, client, writes) <- milestoneWriterFixture original True
            selected <- defaultSelection snapshot
            result <- try (reconcile selected client spec fixtureTarget snapshot)
            failureCodeOf result `shouldBe` Just 1
            readIORef writes `shouldReturn` []
        it "rejects success when GitHub silently ignores milestone assignment" $ withParentWriterSnapshot $ \spec original -> do
            (snapshot, client, writes) <- milestoneWriterFixture original False
            selected <- defaultSelection snapshot
            result <- try (reconcile selected client spec fixtureTarget snapshot)
            failureCodeOf result `shouldBe` Just 3
            calls <- readIORef writes
            length calls `shouldBe` 1
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
        it "projects a migrated v2 envelope without interpreting consumer records" $ withSystemTempDirectory "myque-gh-v2-source" $ \root -> do
            initializeRepo root
            let path = root </> ".tasks" </> "items" </> "019a10d8-8d48-7b77-a414-f95ab7af31be.md"
            writeFile path itemOne
            git root ["add", "."]
            gitEnv root ["commit", "-m", "legacy"]
            before <- withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) pure
            let migrated = T.replace "schema: work-item/v1" "schema: work-item/v2\nexample:\n  opaque: retained" (T.pack itemOne)
            writeFile path (T.unpack migrated)
            git root ["add", "."]
            gitEnv root ["commit", "-m", "envelope migration"]
            withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) $ \after -> do
                let project snapshot = [projectIssue (ProjectionContext snapshot fixtureTarget Map.empty) item [] | item <- storeItems (snapshotStore snapshot)]
                map desiredUuid (project after) `shouldBe` map desiredUuid (project before)
                map desiredBody (project after) `shouldBe` map desiredBody (project before)
        it "loads terminal-only commits offline and ignores working-tree corruption" $ withSystemTempDirectory "myque-gh-terminal-source" $ \root -> do
            _ <- initLayout root
            uuid <- fixtureUuid
            created <- either fail pure (parseTimestamp "2026-08-20T06:21:00Z")
            closed <- either fail pure (parseTimestamp "2026-08-26T06:21:00Z")
            let item = (newWorkItem uuid Task created "retired task"){itemState = Done, itemClosed = Just closed}
                path = root </> ".tasks" </> "terminal" </> T.unpack (T.pack (show uuid)) <> ".json"
                history = History (T.replicate 40 "c") (T.replicate 40 "a") (canonicalRetiredPath uuid) (T.replicate 64 "b")
            createDirectoryIfMissing True (root </> ".tasks" </> "terminal")
            writeFile path (T.unpack (encodeTerminal (TerminalRecord item history "verified" (canonicalRetiredPath uuid))))
            git root ["init", "-q"]
            git root ["add", "."]
            gitEnv root ["commit", "-m", "terminal fixture"]
            writeFile path "corrupt working tree"
            withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) $ \snapshot -> do
                let store = snapshotStore snapshot
                Map.lookup uuid (storeTerminals store) `shouldSatisfy` maybe False ((== history) . terminalHistory)
                fmap itemState (Map.lookup uuid (storeById store)) `shouldBe` Just Done
                github <- discoverGithub (discoveryClient [corePr 1 "OPEN" False (Just "fork/repo") (trailer [uuid])] (factsResponse [])) fixtureTarget snapshot
                Map.member uuid (githubPrsByUuid github) `shouldBe` True
                prepared <- prepareBodies (Just (BodyRenderer "false" [])) snapshot
                snapshotRenderedBodies prepared `shouldBe` Map.empty
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
fixtureGithub issues labels = GithubSnapshot (RepositoryMeta 1 "owner/repo" False True) (Map.elems issues) labels issues Map.empty [] [] Map.empty

fixtureIssue :: Int -> Uuid -> WorkItem -> GithubIssue
fixtureIssue number value item = GithubIssue (fromIntegral number) number ("node" <> T.pack (show number)) "bot" (renderIssueIdentity value <> itemBody item) (itemTitleText item) IssueOpen Nothing Set.empty "now" Nothing Nothing Nothing

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

-- The list shows no membership; the single-issue read can expose a human race.
-- Writes succeed without applying membership, as GitHub can do for weak tokens.
milestoneWriterFixture :: Snapshot -> Bool -> IO (Snapshot, GithubClient, IORef [[String]])
milestoneWriterFixture original humanRace = do
    parent <- fixtureUuid
    child <- otherUuid
    writes <- newIORef []
    let snapshot = adjustSnapshotItem parent (\item -> item{itemKind = Milestone}) original
        items = storeById (snapshotStore snapshot)
        projection = ProjectionContext snapshot fixtureTarget (Map.fromList [(parent, 7), (child, 8)])
        desired = projectMilestone projection (items Map.! parent)
        milestone = GithubMilestone 41 "bot" (desiredMilestoneTitle desired) (desiredMilestoneDescription desired) (desiredMilestoneState desired)
        parentIssue = issueFromDesired 7 (projectIssue projection (items Map.! parent) [])
        childIssue = (issueFromDesired 8 (projectIssue projection (items Map.! child) [])){githubIssueParent = Just (GithubIssueRef "owner" "repo" 7)}
        actualChild = if humanRace then childIssue{githubIssueMilestone = Just (GithubMilestone 99 "human" "human release" "" IssueOpen)} else childIssue
        labels = Set.toAscList (githubIssueLabels parentIssue `Set.union` githubIssueLabels childIssue)
        client = scriptedClient $ \_ args _ ->
            if isWriteCall args
                then do
                    modifyIORef' writes (<> [args])
                    pure (okIncluded (issueValue actualChild))
                else
                    pure $
                        if any (isInfixOfArg "/milestones?") args
                            then jsonResult (Array (pure (milestoneValue milestone)))
                            else
                                if "/repos/owner/repo/milestones/41" `elem` args
                                    then okIncluded (milestoneValue milestone)
                                    else
                                        if "/repos/owner/repo/milestones/99" `elem` args
                                            then maybe (okIncluded Null) (okIncluded . milestoneValue) (githubIssueMilestone actualChild)
                                            else parentWriterResponse [parentIssue, childIssue] labels actualChild args
    pure (snapshot, client, writes)

selectionFor :: Text -> Snapshot -> IO (Set.Set Uuid)
selectionFor expression snapshot = do
    query <- either fail pure (parseQuery expression)
    either (fail . T.unpack) pure (selectProjection query snapshot)

defaultSelection :: Snapshot -> IO (Set.Set Uuid)
defaultSelection = selectionFor defaultProjectionQuery

blankIssue :: Int -> Uuid -> GithubIssue
blankIssue number value = GithubIssue (fromIntegral number) number ("node" <> T.pack (show number)) "bot" (renderIssueIdentity value) "" IssueOpen Nothing Set.empty "now" Nothing Nothing Nothing

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
    | any (isInfixOfArg "/milestones?state=all") args = jsonResult (Array mempty)
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
        , "milestone" .= fmap milestoneValue (githubIssueMilestone issue)
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
    | any (isInfixOfArg "/milestones?state=all") args = jsonResult (Array mempty)
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
    | any (isInfixOfArg "/milestones?state=all") args = jsonResult (Array mempty)
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
discoveryClient = discoveryClientWithIssues []

discoveryClientWithIssues :: [GithubIssue] -> [Value] -> Value -> GithubClient
discoveryClientWithIssues issues = discoveryClientWithMilestones issues []

discoveryClientWithMilestones :: [GithubIssue] -> [GithubMilestone] -> [Value] -> Value -> GithubClient
discoveryClientWithMilestones issues milestones prs facts = scriptedClient $ \_ args _ -> pure $ case args of
    _ | "--include" `elem` args && "/repos/owner/repo" `elem` args -> okIncluded repositoryValue
    _ | any (isInfixOfArg "/issues?") args -> jsonResult (Array (foldMap (pure . issueValue) issues))
    _ | any (isInfixOfArg "/labels?") args -> jsonResult (Array mempty)
    _ | any (isInfixOfArg "/milestones?state=all") args -> jsonResult (Array (foldMap (pure . milestoneValue) milestones))
    _ | "graphql" `elem` args && any (isInfixOfArg "statusCheckRollup") args -> jsonResult facts
    _ | "graphql" `elem` args -> jsonResult (object ["data" .= object ["repository" .= object ["pullRequests" .= object ["nodes" .= prs, "pageInfo" .= object ["hasNextPage" .= False, "endCursor" .= Null]]]]])
    _ -> CommandResult (ExitFailure 1) "" "unexpected fake gh call"

milestoneValue :: GithubMilestone -> Value
milestoneValue milestone =
    object
        [ "number" .= githubMilestoneNumber milestone
        , "creator" .= object ["login" .= githubMilestoneAuthor milestone]
        , "title" .= githubMilestoneTitle milestone
        , "description" .= githubMilestoneDescription milestone
        , "state" .= issueStateValue (githubMilestoneState milestone)
        ]

managedMilestone :: Int -> Uuid -> Text -> IssueState -> GithubMilestone
managedMilestone number value title = GithubMilestone number "bot" title (renderMilestoneIdentity value)

withMilestones :: [(Uuid, GithubMilestone)] -> GithubSnapshot -> GithubSnapshot
withMilestones milestones github = github{githubMilestones = map snd milestones, githubMilestoneByUuid = Map.fromList milestones}

adjustSnapshotItem :: Uuid -> (WorkItem -> WorkItem) -> Snapshot -> Snapshot
adjustSnapshotItem value change snapshot = snapshot{snapshotStore = store{storeById = Map.adjust change value (storeById store)}}
  where
    store = snapshotStore snapshot

milestoneChain :: Kind -> IO Snapshot
milestoneChain middleKind = withSystemTempDirectory "myque-gh-milestone-chain" $ \root -> do
    layout <- initLayout root
    created <- either fail pure (parseTimestamp "2026-08-20T06:21:00Z")
    outer <- fixtureUuid
    middle <- otherUuid
    child <- thirdUuid
    mapM_
        (saveItem layout)
        [ newWorkItem outer Milestone created "outer"
        , (newWorkItem middle middleKind created "middle"){itemParent = Just outer}
        , (newWorkItem child Task created "child"){itemParent = Just middle}
        ]
    git root ["init", "-q"]
    git root ["add", "."]
    gitEnv root ["commit", "-m", "fixture"]
    withSnapshot (SourceSpec root "HEAD" Nothing Nothing Nothing) pure

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

canonicalRetiredPath :: Uuid -> Text
canonicalRetiredPath value = ".tasks/items/" <> uuidText value <> ".md"

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
