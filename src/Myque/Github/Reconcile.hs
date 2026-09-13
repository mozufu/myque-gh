{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Plan and apply guarded convergence from canonical state to GitHub.
module Myque.Github.Reconcile (
    buildPlan,
    diffIssue,
    renderPlan,
    planIsEmpty,
    applyParentChange,
    reconcile,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Parser, parseEither)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Github.Github (
    discoverGithub,
    encodePathSegment,
    getIssue,
    getRepository,
    parseIssue,
    repoPath,
    restSingle,
 )
import Myque.Github.Markers (MarkerError (..), parseIssueIdentity)
import Myque.Github.Projection
import Myque.Github.Source (resolveSourceRef)
import Myque.Github.Types
import Myque.Item (WorkItem (..), isTerminal)
import Myque.Store (Store (..))
import Myque.Uuid (Uuid, uuidText)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory)
import System.FileLock (SharedExclusive (Exclusive), tryLockFile, unlockFile)
import System.FilePath (takeDirectory, (</>))

-- | Construct the complete deterministic convergence plan.
buildPlan :: Target -> Snapshot -> GithubSnapshot -> Either [Conflict] Plan
buildPlan target snapshot github = do
    tagCollisionCheck selectedItems
    parentReferenceCheck selected
    let issueNumbers = Map.map githubIssueNumber (githubIssueByUuid github)
        context = ProjectionContext snapshot target issueNumbers
        desired = Map.fromList [(uuid, projectIssue context item (Map.findWithDefault [] uuid (githubPrsByUuid github))) | (uuid, item) <- selected]
        creates = Map.filterWithKey (\uuid _ -> not (Map.member uuid (githubIssueByUuid github))) desired
        changes =
            Map.fromList
                [ (githubIssueNumber issue, (uuid, desiredDisplay want, drift))
                | (uuid, want) <- Map.toAscList desired
                , Just issue <- [Map.lookup uuid (githubIssueByUuid github)]
                , let drift = diffIssue want issue
                , not (null drift)
                ]
        parentChanges =
            Map.fromList
                [ (uuid, (renderDisplay snapshot item, itemParent item))
                | (uuid, item) <- selected
                , parentPending target issueNumbers (Map.lookup uuid (githubIssueByUuid github)) (itemParent item)
                ]
        desiredManaged = Set.unions (map desiredLabels (Map.elems desired))
        existingByFold = Map.fromList [(T.toCaseFold (githubLabelName label), githubLabelName label) | label <- githubLabels github]
        labelCreates = sortOn T.toCaseFold [name | name <- Set.toList desiredManaged, not (Map.member (T.toCaseFold name) existingByFold)]
        orphanWarnings =
            [ Warning "orphaned-projection" ("issue #" <> T.pack (show (githubIssueNumber issue)) <> " projects missing canonical " <> uuidText uuid <> "; left unchanged")
            | (uuid, issue) <- Map.toAscList (githubIssueByUuid github)
            , not (Map.member uuid canonical)
            ]
    pure Plan{planLabelCreates = labelCreates, planIssueCreates = creates, planIssueChanges = changes, planParentChanges = parentChanges, planWarnings = githubWarnings github <> orphanWarnings}
  where
    canonical = storeById (snapshotStore snapshot)
    initiallyProjected = Set.fromList [uuid | (uuid, item) <- Map.toAscList canonical, not (isTerminal (itemState item)) || Map.member uuid (githubIssueByUuid github)]
    projected = parentClosure canonical initiallyProjected
    selected = [(uuid, item) | (uuid, item) <- Map.toAscList canonical, Set.member uuid projected]
    selectedItems = map snd selected

parentClosure :: Map.Map Uuid WorkItem -> Set.Set Uuid -> Set.Set Uuid
parentClosure canonical initial = go initial (Set.toList initial)
  where
    go projected [] = projected
    go projected (uuid : rest) = case itemParent =<< Map.lookup uuid canonical of
        Nothing -> go projected rest
        Just parent
            | Set.member parent projected -> go projected rest
            | otherwise -> go (Set.insert parent projected) (parent : rest)

expectedParentRef :: Target -> Map.Map Uuid Int -> Maybe Uuid -> Maybe GithubIssueRef
expectedParentRef target numbers parent = do
    uuid <- parent
    number <- Map.lookup uuid numbers
    pure (GithubIssueRef (targetOwner target) (targetRepo target) number)

-- | Decide whether a parent link is still pending, including issues not created yet.
parentPending :: Target -> Map.Map Uuid Int -> Maybe GithubIssue -> Maybe Uuid -> Bool
parentPending target numbers issue parent = case issue of
    -- The child issue does not exist yet, so apply will link it after creation.
    Nothing -> isJust parent
    -- A desired parent without a number is an issue this run still has to create.
    Just current
        | isJust parent && isNothing expected -> True
        | otherwise -> githubIssueParent current /= expected
  where
    expected = expectedParentRef target numbers parent

parentReferenceCheck :: [(Uuid, WorkItem)] -> Either [Conflict] ()
parentReferenceCheck selected = case missing of
    [] -> Right ()
    values -> Left [Conflict "unprojected-parent" (uuidText child <> " references parent " <> uuidText parent <> " that has no projected issue") | (child, parent) <- values]
  where
    projected = Set.fromList (map fst selected)
    missing = [(uuid, parent) | (uuid, item) <- selected, Just parent <- [itemParent item], not (Set.member parent projected)]

tagCollisionCheck :: [WorkItem] -> Either [Conflict] ()
tagCollisionCheck items = case collisions of
    [] -> Right ()
    values -> Left [Conflict "tag-case-collision" ("canonical tags collide case-insensitively: " <> T.intercalate ", " (Set.toAscList variants)) | variants <- values]
  where
    grouped = Map.fromListWith Set.union [(T.toCaseFold tag, Set.singleton tag) | item <- items, tag <- itemTags item]
    collisions = [variants | variants <- Map.elems grouped, Set.size variants > 1]

-- | Compare only issue fields owned by the projection.
diffIssue :: DesiredIssue -> GithubIssue -> [IssueChange]
diffIssue desired actual =
    titleChange <> bodyChange <> stateChange <> removes <> adds
  where
    titleChange = [SetTitle (desiredTitle desired) | desiredTitle desired /= githubIssueTitle actual]
    bodyChange = [SetBody (desiredBody desired) | desiredBody desired /= githubIssueBody actual]
    stateChange =
        [ SetState (desiredState desired) (desiredReason desired)
        | desiredState desired /= githubIssueState actual
            || (desiredState desired == IssueClosed && desiredReason desired /= githubIssueReason actual)
        ]
    desiredByFold = Map.fromList [(T.toCaseFold name, name) | name <- Set.toList (desiredLabels desired)]
    actualManaged = Map.fromList [(T.toCaseFold name, name) | name <- Set.toList (githubIssueLabels actual), "myque:" `T.isPrefixOf` T.toCaseFold name]
    removes = [RemoveLabel actualName | (folded, actualName) <- Map.toAscList actualManaged, not (Map.member folded desiredByFold)]
    adds = [AddLabel (Map.findWithDefault desiredName folded actualManaged) | (folded, desiredName) <- Map.toAscList desiredByFold, not (Map.member folded actualManaged)]

-- | Render a stable human-readable plan.
renderPlan :: Target -> Snapshot -> Plan -> Text
renderPlan target snapshot plan =
    "Target: "
        <> targetOwner target
        <> "/"
        <> targetRepo target
        <> "\n"
        <> "Source: "
        <> snapshotSha snapshot
        <> " (committed snapshot; uncommitted edits excluded)\n"
        <> operations
  where
    operations
        | planIsEmpty plan = "No changes.\n"
        | otherwise =
            T.unlines $
                map ("+ label " <>) (planLabelCreates plan)
                    <> ["+ issue " <> uuidText uuid <> " " <> desiredDisplay desired | (uuid, desired) <- Map.toAscList (planIssueCreates plan)]
                    <> concatMap renderIssue (Map.toAscList (planIssueChanges plan))
                    <> ["~ issue " <> uuidText uuid <> " " <> display <> " parent " <> maybe "none" uuidText parent | (uuid, (display, parent)) <- Map.toAscList (planParentChanges plan)]
    renderIssue (number, (uuid, _display, changes)) = map (renderChange number uuid) changes
    renderChange number uuid change = case change of
        SetTitle _ -> field "title"
        SetBody _ -> field "body"
        SetState _ _ -> field "state"
        AddLabel name -> "+ issue #" <> n <> " " <> uuidText uuid <> " label " <> name
        RemoveLabel name -> "- issue #" <> n <> " " <> uuidText uuid <> " label " <> name
      where
        n = T.pack (show number)
        field name = "~ issue #" <> n <> " " <> uuidText uuid <> " " <> name

-- | Test whether a plan contains no mutations.
planIsEmpty :: Plan -> Bool
planIsEmpty plan = null (planLabelCreates plan) && Map.null (planIssueCreates plan) && Map.null (planIssueChanges plan) && Map.null (planParentChanges plan)

-- | Apply a guarded plan, rediscover GitHub, and require convergence.
reconcile :: GithubClient -> SourceSpec -> Target -> Snapshot -> IO Plan
reconcile client spec target snapshot = withTargetLock target $ do
    initialGithub <- discoverGithub client target snapshot
    initialPlan <- either conflicts pure (buildPlan target snapshot initialGithub)
    let initialRepository = githubRepository initialGithub
    forM_ (planLabelCreates initialPlan) $ \name -> do
        guardTarget client spec snapshot target initialRepository
        _ <- restSingle client "POST" (repoPath target <> "/labels") (Just (object ["name" .= name, "color" .= ("ededed" :: Text), "description" .= ("Managed by myque-gh." :: Text)]))
        pure ()
    created <- foldM (createIssue client spec target snapshot initialRepository) (githubIssueByUuid initialGithub) (Map.toAscList (planIssueCreates initialPlan))
    refreshed <- discoverGithub client target snapshot
    let createdLabels = [GithubLabel name "ededed" (Just "Managed by myque-gh.") | name <- planLabelCreates initialPlan]
        -- List endpoints lag behind writes, so this run's creates stay visible.
        withCreates observed =
            observed
                { githubLabels = mergeLabels createdLabels (githubLabels observed)
                , githubIssueByUuid = Map.union (githubIssueByUuid observed) created
                }
        refreshedWithCreates = withCreates refreshed
    postCreatePlan <- either conflicts pure (buildPlan target snapshot refreshedWithCreates)
    forM_ (Map.toAscList (planIssueChanges postCreatePlan)) $ \(number, (uuid, _display, _changes)) -> do
        guardTarget client spec snapshot target initialRepository
        current <- getIssue client target number
        verifyManaged target uuid current
        applyIssueDiff client target number (lookupDesired target snapshot refreshedWithCreates uuid) current
    refreshedFields <- withCreates <$> discoverGithub client target snapshot
    parentPlan <- either conflicts pure (buildPlan target snapshot refreshedFields)
    forM_ (parentOrder snapshot (Map.keysSet (planParentChanges parentPlan))) $ \uuid -> do
        guardTarget client spec snapshot target initialRepository
        let desiredParent = snd (planParentChanges parentPlan Map.! uuid)
        participants <- refreshParentParticipants client target refreshedFields uuid desiredParent
        applyParentChange client target participants uuid desiredParent
    finalObserved <- withCreates <$> discoverGithub client target snapshot
    verified <- traverse (verifyCurrentIssue client target snapshot finalObserved) (Map.toAscList (githubIssueByUuid finalObserved))
    let finalGithub = finalObserved{githubIssueByUuid = Map.fromList verified}
    finalPlan <- either conflicts pure (buildPlan target snapshot finalGithub)
    unless (planIsEmpty finalPlan) (throwIO Failure{failureCode = 3, failureDiagnostic = "not-converged\n" <> renderPlan target snapshot finalPlan})
    pure initialPlan

verifyCurrentIssue :: GithubClient -> Target -> Snapshot -> GithubSnapshot -> (Uuid, GithubIssue) -> IO (Uuid, GithubIssue)
verifyCurrentIssue client target snapshot github (uuid, prior) = case Map.lookup uuid (storeById (snapshotStore snapshot)) of
    Nothing -> pure (uuid, prior)
    Just _ -> do
        current <- getIssue client target (githubIssueNumber prior)
        verifyManaged target uuid current
        let desired = lookupDesired target snapshot github uuid
            drift = diffIssue desired current
        unless (null drift) (throwIO Failure{failureCode = 3, failureDiagnostic = "not-converged issue #" <> T.pack (show (githubIssueNumber current)) <> " " <> uuidText uuid})
        pure (uuid, current)

parentOrder :: Snapshot -> Set.Set Uuid -> [Uuid]
parentOrder snapshot pending = sortOn depth (Set.toAscList pending)
  where
    store = storeById (snapshotStore snapshot)
    depth uuid = case itemParent =<< Map.lookup uuid store of
        Nothing -> 0 :: Int
        Just parent -> 1 + depth parent

-- | Apply one native GitHub parent relationship replacement or removal.
applyParentChange :: GithubClient -> Target -> GithubSnapshot -> Uuid -> Maybe Uuid -> IO ()
applyParentChange client target github childUuid desiredParent = do
    child <- requireIssue childUuid
    case desiredParent of
        Nothing -> forM_ (githubIssueParent child) $ \current -> do
            _ <- restSingle client "DELETE" (issueRefPath current <> "/sub_issue") (Just (object ["sub_issue_id" .= githubIssueId child]))
            pure ()
        Just parentUuid -> do
            parent <- requireIssue parentUuid
            let desiredRef = GithubIssueRef (targetOwner target) (targetRepo target) (githubIssueNumber parent)
            when (githubIssueParent child /= Just desiredRef) $ do
                _ <- restSingle client "POST" (repoPath target <> "/issues/" <> number parent <> "/sub_issues") (Just (object ["sub_issue_id" .= githubIssueId child, "replace_parent" .= True]))
                pure ()
  where
    requireIssue uuid = maybe (throwIO Failure{failureCode = 1, failureDiagnostic = "cannot project parent before both issues exist: " <> uuidText uuid}) pure (Map.lookup uuid (githubIssueByUuid github))
    issueRefPath reference = "/repos/" <> encodePathSegment (githubIssueRefOwner reference) <> "/" <> encodePathSegment (githubIssueRefRepo reference) <> "/issues/" <> T.pack (show (githubIssueRefNumber reference))
    number = T.pack . show . githubIssueNumber

-- | Re-read and re-verify ownership of the issues a parent mutation will touch.
refreshParentParticipants :: GithubClient -> Target -> GithubSnapshot -> Uuid -> Maybe Uuid -> IO GithubSnapshot
refreshParentParticipants client target github childUuid desiredParent =
    foldM refreshOne github (childUuid : maybe [] pure desiredParent)
  where
    refreshOne observed uuid = case Map.lookup uuid (githubIssueByUuid observed) of
        Nothing -> pure observed
        Just prior -> do
            current <- getIssue client target (githubIssueNumber prior)
            verifyManaged target uuid current
            pure observed{githubIssueByUuid = Map.insert uuid current (githubIssueByUuid observed)}

mergeLabels :: [GithubLabel] -> [GithubLabel] -> [GithubLabel]
mergeLabels created existing = Map.elems (Map.fromList [(T.toCaseFold (githubLabelName label), label) | label <- existing <> created])

lookupDesired :: Target -> Snapshot -> GithubSnapshot -> Uuid -> DesiredIssue
lookupDesired target snapshot github uuid =
    let numbers = Map.map githubIssueNumber (githubIssueByUuid github)
        context = ProjectionContext snapshot target numbers
        item = storeById (snapshotStore snapshot) Map.! uuid
     in projectIssue context item (Map.findWithDefault [] uuid (githubPrsByUuid github))

createIssue :: GithubClient -> SourceSpec -> Target -> Snapshot -> RepositoryMeta -> Map.Map Uuid GithubIssue -> (Uuid, DesiredIssue) -> IO (Map.Map Uuid GithubIssue)
createIssue client spec target snapshot repository mapping (uuid, desired) = do
    guardTarget client spec snapshot target repository
    (_, value) <- restSingle client "POST" (repoPath target <> "/issues") (Just (object ["title" .= desiredTitle desired, "body" .= desiredBody desired, "labels" .= Set.toAscList (desiredLabels desired)]))
    issue <- either (\message -> throwIO Failure{failureCode = 3, failureDiagnostic = "invalid created issue response: " <> T.pack message}) pure (parseEither parseCreatedIssue value)
    verifyManaged target uuid issue
    pure (Map.insert uuid issue mapping)

parseCreatedIssue :: Value -> Parser GithubIssue
parseCreatedIssue = parseIssue

applyIssueDiff :: GithubClient -> Target -> Int -> DesiredIssue -> GithubIssue -> IO ()
applyIssueDiff client target number desired current = do
    let changes = diffIssue desired current
        scalarFields = concatMap patchField changes
    unless (null scalarFields) $ do
        _ <- restSingle client "PATCH" (repoPath target <> "/issues/" <> T.pack (show number)) (Just (object scalarFields))
        pure ()
    forM_ changes $ \case
        AddLabel name -> do
            _ <- restSingle client "POST" (repoPath target <> "/issues/" <> T.pack (show number) <> "/labels") (Just (object ["labels" .= [name]]))
            pure ()
        RemoveLabel name -> do
            _ <- restSingle client "DELETE" (repoPath target <> "/issues/" <> T.pack (show number) <> "/labels/" <> encodePathSegment name) Nothing
            pure ()
        _ -> pure ()
  where
    patchField change = case change of
        SetTitle title -> ["title" .= title]
        SetBody body -> ["body" .= body]
        SetState state reason -> ["state" .= issueStateText state, "state_reason" .= fmap reasonText reason]
        _ -> []
    issueStateText IssueOpen = ("open" :: Text)
    issueStateText IssueClosed = "closed"
    reasonText Completed = ("completed" :: Text)
    reasonText NotPlanned = "not_planned"

guardTarget :: GithubClient -> SourceSpec -> Snapshot -> Target -> RepositoryMeta -> IO ()
guardTarget client spec snapshot target expected = do
    current <- getRepository client target
    unless
        (repositoryId current == repositoryId expected && T.toCaseFold (repositoryNameWithOwner current) == T.toCaseFold (repositoryNameWithOwner expected))
        (throwIO Failure{failureCode = 3, failureDiagnostic = "target-mismatch during mutation guard"})
    sha <- resolveSourceRef (sourceRoot spec) (sourceRef spec)
    unless (sha == snapshotSha snapshot) (throwIO Failure{failureCode = 3, failureDiagnostic = "source-ref-moved"})

verifyManaged :: Target -> Uuid -> GithubIssue -> IO ()
verifyManaged target uuid issue = do
    unless
        (T.toCaseFold (githubIssueAuthor issue) `Set.member` targetIssueAuthors target)
        (throwIO Failure{failureCode = 1, failureDiagnostic = "created/current issue author is not trusted: " <> githubIssueAuthor issue})
    case parseIssueIdentity (githubIssueBody issue) of
        Right (Just actual) | actual == uuid -> pure ()
        Right _ -> throwIO Failure{failureCode = 1, failureDiagnostic = "issue identity changed"}
        Left (MarkerError message) -> throwIO Failure{failureCode = 1, failureDiagnostic = "issue identity malformed: " <> message}

withTargetLock :: Target -> IO a -> IO a
withTargetLock target action = do
    cache <- getXdgDirectory XdgCache ("myque-gh" </> "locks" </> "github.com" </> T.unpack (T.toCaseFold (targetOwner target)))
    let path = cache </> T.unpack (T.toCaseFold (targetRepo target)) <> ".lock"
    createDirectoryIfMissing True (takeDirectory path)
    locked <- tryLockFile path Exclusive
    case locked of
        Nothing -> throwIO Failure{failureCode = 1, failureDiagnostic = "writer-busy"}
        Just lock -> bracket (pure lock) unlockFile (const action)

conflicts :: [Conflict] -> IO a
conflicts values = throwIO Failure{failureCode = 1, failureDiagnostic = T.intercalate "\n" [conflictCode value <> ": " <> conflictDetail value | value <- values]}
