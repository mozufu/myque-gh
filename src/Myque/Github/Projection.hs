{-# LANGUAGE OverloadedStrings #-}

-- | Render canonical work items into managed GitHub issue fields.
module Myque.Github.Projection (
    ProjectionContext (..),
    projectIssue,
    projectMilestone,
    nearestMilestone,
    managedLabels,
    renderDisplay,
) where

import Data.Char (isControl)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Github.Github (encodePathSegment)
import Myque.Github.Markers (renderIssueIdentity, renderMilestoneIdentity)
import Myque.Github.Types
import Myque.Graph (dependenciesOf, isReady)
import Myque.Item (
    Kind (Milestone),
    State (..),
    WorkItem (..),
    isTerminal,
    itemTitle,
    kindText,
    stateText,
 )
import Myque.Render (label)
import Myque.Store (Store (..))
import Myque.Uuid (Uuid, uuidText)

-- | Immutable context needed to render links and relationship displays.
data ProjectionContext = ProjectionContext
    { projectionSnapshot :: Snapshot
    , projectionTarget :: Target
    , projectionIssueNumbers :: Map.Map Uuid Int
    }

-- | Project one canonical item and its linked pull requests into owned issue fields.
projectIssue :: ProjectionContext -> WorkItem -> [LinkedPr] -> DesiredIssue
projectIssue context item linkedPrs =
    DesiredIssue
        { desiredUuid = itemId item
        , desiredDisplay = display
        , desiredTitle = display <> " · " <> itemTitle item
        , desiredBody = renderBody context item linkedPrs
        , desiredState = issueState
        , desiredReason = closeReason
        , desiredLabels = managedLabels item
        }
  where
    display = renderDisplay (projectionSnapshot context) item
    (issueState, closeReason) = case itemState item of
        Done -> (IssueClosed, Just Completed)
        Cancelled -> (IssueClosed, Just NotPlanned)
        _ -> (IssueOpen, Nothing)

{- | Keep native grouping separate from the milestone's discussion/parent issue.
Due dates have no canonical field and are deliberately not projected.
-}
projectMilestone :: ProjectionContext -> WorkItem -> DesiredMilestone
projectMilestone context item =
    DesiredMilestone
        { desiredMilestoneUuid = itemId item
        , desiredMilestoneTitle = itemTitle item
        , desiredMilestoneDescription =
            renderMilestoneIdentity (itemId item)
                <> "Managed by myque. Title, description and state are projected; due date remains human-owned.\n\n"
                <> "**Canonical:** "
                <> renderCanonical context item
                <> "\n\n**Myque state:** "
                <> stateText (itemState item)
                <> "\n\nGitHub progress counts closed issues; it is not myque completion evidence.\n"
                <> itemBody item
        , desiredMilestoneState = if isTerminal (itemState item) then IssueClosed else IssueOpen
        }

{- | Native milestones are flat: use the nearest strict milestone ancestor.
A container's own issue never counts toward its own milestone's progress.
-}
nearestMilestone :: Snapshot -> WorkItem -> Maybe Uuid
nearestMilestone snapshot item = itemParent item >>= ascend
  where
    canonical = storeById (snapshotStore snapshot)
    ascend uuid = do
        parent <- Map.lookup uuid canonical
        if itemKind parent == Milestone then Just uuid else itemParent parent >>= ascend

-- | Compute the complete set of labels owned by @myque-gh@.
managedLabels :: WorkItem -> Set Text
managedLabels item =
    Set.fromList
        ( ["myque:kind:" <> kindText (itemKind item), "myque:state:" <> stateText (itemState item)]
            <> map ("myque:tag:" <>) (itemTags item)
        )

-- | Render the stable human-facing selector for a canonical work item.
renderDisplay :: Snapshot -> WorkItem -> Text
renderDisplay snapshot item = label (snapshotAbbrev snapshot) item

renderBody :: ProjectionContext -> WorkItem -> [LinkedPr] -> Text
renderBody context item linkedPrs =
    renderIssueIdentity (itemId item)
        <> "Managed by myque. Title, body, myque:* labels, work state, parent and canonical milestone grouping are projected; use comments for discussion.\n\n"
        <> "**Myque:** "
        <> markdown display
        <> " — <code>"
        <> html (uuidText (itemId item))
        <> "</code>\n"
        <> "**Kind:** "
        <> markdown (kindText (itemKind item))
        <> " · **State:** "
        <> markdown (stateText (itemState item))
        <> " · **Ready:** "
        <> yesNo ready
        <> "\n"
        <> "**Tags:** "
        <> renderTags (itemTags item)
        <> "\n"
        <> "**Canonical:** "
        <> renderCanonical context item
        <> "\n\n"
        <> "## Relationships\n"
        <> renderParent context item
        <> renderDependencies context item
        <> "\n## Implementation\n"
        <> renderPullRequests context linkedPrs
        <> "\n---\n"
        <> itemBody item
  where
    snapshot = projectionSnapshot context
    store = snapshotStore snapshot
    display = renderDisplay snapshot item
    ready = isReady store (snapshotEdges snapshot) item

yesNo :: Bool -> Text
yesNo True = "yes"
yesNo False = "no"

renderTags :: [Text] -> Text
renderTags [] = "none"
renderTags tags = T.intercalate ", " ["<code>" <> html tag <> "</code>" | tag <- sort tags]

renderCanonical :: ProjectionContext -> WorkItem -> Text
renderCanonical context item = case (sourceLinkRepo spec, sourceLinkBranch spec) of
    (Just (owner, repo), Just branch) ->
        "[" <> markdown path <> "](https://github.com/" <> encodePathSegment owner <> "/" <> encodePathSegment repo <> "/blob/" <> encodePathSegment branch <> "/" <> encodePath path <> ")"
    _ -> markdown path <> " — <code>" <> html (uuidText (itemId item)) <> "</code>"
  where
    snapshot = projectionSnapshot context
    spec = snapshotSource snapshot
    path = T.pack (Map.findWithDefault (".tasks/items/" <> T.unpack (uuidText (itemId item)) <> ".md") (itemId item) (snapshotSourcePaths snapshot))

renderParent :: ProjectionContext -> WorkItem -> Text
renderParent context item = case itemParent item of
    Nothing -> "- Parent: none\n"
    Just uuid -> "- Parent: " <> renderRelation context uuid <> "\n"

renderDependencies :: ProjectionContext -> WorkItem -> Text
renderDependencies context item = case dependenciesOf (snapshotEdges snapshot) item of
    [] -> "- Dependencies: none\n"
    dependencies -> "- Dependencies:\n" <> T.concat ["  - " <> renderRelation context uuid <> "\n" | uuid <- dependencies]
  where
    snapshot = projectionSnapshot context

renderRelation :: ProjectionContext -> Uuid -> Text
renderRelation context uuid = case Map.lookup uuid (storeById store) of
    Nothing -> "<code>" <> html (uuidText uuid) <> "</code>"
    Just related -> markdown display <> " — " <> markdown (stateText (itemState related)) <> " — " <> link
      where
        display = renderDisplay snapshot related
        link = case Map.lookup uuid (projectionIssueNumbers context) of
            Just number -> "[#" <> T.pack (show number) <> "](https://github.com/" <> encodePathSegment (targetOwner target) <> "/" <> encodePathSegment (targetRepo target) <> "/issues/" <> T.pack (show number) <> ")"
            Nothing -> fallback related
  where
    snapshot = projectionSnapshot context
    store = snapshotStore snapshot
    target = projectionTarget context
    fallback related = case (sourceLinkRepo spec, sourceLinkBranch spec) of
        (Just (owner, repo), Just branch) ->
            let path = T.pack (Map.findWithDefault (".tasks/items/" <> T.unpack (uuidText uuid) <> ".md") uuid (snapshotSourcePaths snapshot))
             in "[" <> markdown path <> "](https://github.com/" <> encodePathSegment owner <> "/" <> encodePathSegment repo <> "/blob/" <> encodePathSegment branch <> "/" <> encodePath path <> ")"
        _ -> "<code>" <> html (uuidText (itemId related)) <> "</code>"
    spec = snapshotSource snapshot

renderPullRequests :: ProjectionContext -> [LinkedPr] -> Text
renderPullRequests _ [] = "No linked pull requests.\n"
renderPullRequests context prs =
    T.concat (map renderOne prs)
        <> "\nCI/review facts are GitHub observations for the displayed head; they are not myque completion evidence.\n"
  where
    target = projectionTarget context
    renderOne pr =
        "- [#"
            <> number
            <> "](https://github.com/"
            <> encodePathSegment (targetOwner target)
            <> "/"
            <> encodePathSegment (targetRepo target)
            <> "/pull/"
            <> number
            <> ") — "
            <> markdown (linkedPrTitle pr)
            <> " — "
            <> lifecycleText (linkedPrLifecycle pr)
            <> readiness pr
            <> " · CI "
            <> ciText (linkedPrCi pr)
            <> " · review "
            <> reviewText (linkedPrReview pr)
            <> "\n"
            <> "  - Head: <code>"
            <> html (repoText (linkedPrHeadRepo pr) <> ":" <> linkedPrHeadRef pr)
            <> "</code> at <code>"
            <> html (linkedPrHeadSha pr)
            <> "</code>; base: <code>"
            <> html (linkedPrBaseRepo pr <> ":" <> linkedPrBaseRef pr)
            <> "</code>\n"
      where
        number = T.pack (show (linkedPrNumber pr))
        readiness value
            | linkedPrLifecycle value == PrOpen = if linkedPrDraft value then " · draft" else " · ready"
            | otherwise = ""
    repoText = fromMaybe "deleted repository"

lifecycleText :: PrLifecycle -> Text
lifecycleText PrOpen = "open"
lifecycleText PrMerged = "merged"
lifecycleText PrClosedUnmerged = "closed-unmerged"

ciText :: CiState -> Text
ciText CiPassing = "passing"
ciText CiFailing = "failing"
ciText CiPending = "pending"
ciText CiNone = "none"
ciText CiUnknown = "unknown"

reviewText :: ReviewState -> Text
reviewText ReviewApproved = "approved"
reviewText ReviewChangesRequested = "changes-requested"
reviewText ReviewRequired = "required"
reviewText ReviewNone = "none"
reviewText ReviewUnknown = "unknown"

markdown :: Text -> Text
markdown = T.concatMap escape
  where
    escape char
        | isControl char = visible char
        | char `elem` ("\\`*_{}[]<>()#+-.!|" :: String) = "\\" <> T.singleton char
        | char == '&' = "&amp;"
        | char == '<' = "&lt;"
        | char == '>' = "&gt;"
        | otherwise = T.singleton char

html :: Text -> Text
html = T.concatMap $ \char -> case char of
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '"' -> "&quot;"
    _
        | isControl char -> visible char
        | otherwise -> T.singleton char

visible :: Char -> Text
visible '\n' = "\\n"
visible '\r' = "\\r"
visible '\t' = "\\t"
visible char = "\\u{" <> T.pack (show (fromEnum char)) <> "}"

encodePath :: Text -> Text
encodePath = T.intercalate "/" . map encodePathSegment . T.splitOn "/"
