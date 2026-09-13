{-# LANGUAGE OverloadedStrings #-}

-- | Shared values for canonical snapshots, GitHub observations, and reconciliation plans.
module Myque.Github.Types (
    Target (..),
    SourceSpec (..),
    Snapshot (..),
    Failure (..),
    failureExitCode,
    IssueState (..),
    CloseReason (..),
    GithubLabel (..),
    GithubIssueRef (..),
    GithubIssue (..),
    RepositoryMeta (..),
    PrLifecycle (..),
    CiState (..),
    ReviewState (..),
    LinkedPr (..),
    GithubSnapshot (..),
    DesiredIssue (..),
    IssueChange (..),
    Conflict (..),
    Warning (..),
    Plan (..),
    GithubClient (..),
    CommandResult (..),
) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Myque.Graph (Edges)
import Myque.Render (Abbrev)
import Myque.Store (Store)
import Myque.Uuid (Uuid)
import System.Exit (ExitCode (..))

-- | Explicit GitHub target and trusted projection creators.
data Target = Target
    { targetOwner :: Text
    , targetRepo :: Text
    , targetIssueAuthors :: Set Text
    }
    deriving (Eq, Show)

-- | Immutable canonical source selection.
data SourceSpec = SourceSpec
    { sourceRoot :: FilePath
    , sourceRef :: Text
    , sourcePublicationRef :: Maybe Text
    , sourceLinkRepo :: Maybe (Text, Text)
    , sourceLinkBranch :: Maybe Text
    }
    deriving (Eq, Show)

-- | Validated canonical state loaded from one commit.
data Snapshot = Snapshot
    { snapshotSha :: Text
    , snapshotSource :: SourceSpec
    , snapshotStore :: Store
    , snapshotEdges :: Edges
    , snapshotAbbrev :: Abbrev
    , snapshotSourcePaths :: Map Uuid FilePath
    }

-- | A stable CLI failure classification and diagnostic.
data Failure = Failure
    { failureCode :: Int
    , failureDiagnostic :: Text
    }
    deriving (Eq, Show)

instance Exception Failure

-- | Convert a classified failure to its stable process exit code.
failureExitCode :: Failure -> ExitCode
failureExitCode failure = ExitFailure (failureCode failure)

-- | Open or closed GitHub issue state.
data IssueState = IssueOpen | IssueClosed
    deriving (Eq, Ord, Show)

-- | GitHub's semantic reason for a closed issue.
data CloseReason = Completed | NotPlanned
    deriving (Eq, Ord, Show)

-- | GitHub label metadata used during discovery.
data GithubLabel = GithubLabel
    { githubLabelName :: Text
    , githubLabelColor :: Text
    , githubLabelDescription :: Maybe Text
    }
    deriving (Eq, Show)

-- | Stable coordinates for an issue that may live in another repository.
data GithubIssueRef = GithubIssueRef
    { githubIssueRefOwner :: Text
    , githubIssueRefRepo :: Text
    , githubIssueRefNumber :: Int
    }
    deriving (Eq, Ord, Show)

-- | Observed GitHub issue fields owned or consulted by reconciliation.
data GithubIssue = GithubIssue
    { githubIssueId :: Integer
    , githubIssueNumber :: Int
    , githubIssueNodeId :: Text
    , githubIssueAuthor :: Text
    , githubIssueBody :: Text
    , githubIssueTitle :: Text
    , githubIssueState :: IssueState
    , githubIssueReason :: Maybe CloseReason
    , githubIssueLabels :: Set Text
    , githubIssueUpdatedAt :: Text
    , githubIssueUrl :: Maybe Text
    , githubIssueParent :: Maybe GithubIssueRef
    }
    deriving (Eq, Show)

-- | Repository identity and issue availability used by mutation guards.
data RepositoryMeta = RepositoryMeta
    { repositoryId :: Integer
    , repositoryNameWithOwner :: Text
    , repositoryArchived :: Bool
    , repositoryHasIssues :: Bool
    }
    deriving (Eq, Show)

-- | Lifecycle classification for a linked pull request.
data PrLifecycle = PrOpen | PrMerged | PrClosedUnmerged
    deriving (Eq, Ord, Show)

-- | CI rollup observed for a linked pull request head.
data CiState = CiPassing | CiFailing | CiPending | CiNone | CiUnknown
    deriving (Eq, Ord, Show)

-- | Review decision observed for a linked pull request head.
data ReviewState = ReviewApproved | ReviewChangesRequested | ReviewRequired | ReviewNone | ReviewUnknown
    deriving (Eq, Ord, Show)

-- | A pull request linked to one canonical work item.
data LinkedPr = LinkedPr
    { linkedPrNumber :: Int
    , linkedPrTitle :: Text
    , linkedPrLifecycle :: PrLifecycle
    , linkedPrDraft :: Bool
    , linkedPrHeadRepo :: Maybe Text
    , linkedPrHeadRef :: Text
    , linkedPrHeadSha :: Text
    , linkedPrBaseRepo :: Text
    , linkedPrBaseRef :: Text
    , linkedPrCi :: CiState
    , linkedPrReview :: ReviewState
    }
    deriving (Eq, Show)

-- | Complete GitHub observation used to construct a reconciliation plan.
data GithubSnapshot = GithubSnapshot
    { githubRepository :: RepositoryMeta
    , githubIssues :: [GithubIssue]
    , githubLabels :: [GithubLabel]
    , githubIssueByUuid :: Map Uuid GithubIssue
    , githubPrsByUuid :: Map Uuid [LinkedPr]
    , githubWarnings :: [Warning]
    }
    deriving (Eq, Show)

-- | GitHub issue state desired for one canonical work item.
data DesiredIssue = DesiredIssue
    { desiredUuid :: Uuid
    , desiredDisplay :: Text
    , desiredTitle :: Text
    , desiredBody :: Text
    , desiredState :: IssueState
    , desiredReason :: Maybe CloseReason
    , desiredLabels :: Set Text
    }
    deriving (Eq, Show)

-- | One mutable field difference on a managed GitHub issue.
data IssueChange
    = SetTitle Text
    | SetBody Text
    | SetState IssueState (Maybe CloseReason)
    | AddLabel Text
    | RemoveLabel Text
    deriving (Eq, Show)

-- | Configuration conflict that prevents a deterministic plan.
data Conflict = Conflict
    { conflictCode :: Text
    , conflictDetail :: Text
    }
    deriving (Eq, Ord, Show)

-- | Non-fatal observation reported with a plan.
data Warning = Warning
    { warningCode :: Text
    , warningDetail :: Text
    }
    deriving (Eq, Ord, Show)

-- | Ordered mutations required to converge GitHub on canonical state.
data Plan = Plan
    { planLabelCreates :: [Text]
    , planIssueCreates :: Map Uuid DesiredIssue
    , planIssueChanges :: Map Int (Uuid, Text, [IssueChange])
    , planParentChanges :: Map Uuid (Text, Maybe Uuid)
    , planWarnings :: [Warning]
    }
    deriving (Eq, Show)

-- | Captured result of invoking GitHub CLI or Git.
data CommandResult = CommandResult
    { commandExit :: ExitCode
    , commandStdout :: ByteString
    , commandStderr :: ByteString
    }
    deriving (Eq, Show)

-- | GitHub transport executable and injectable argument-vector runner.
data GithubClient = GithubClient
    { githubExecutable :: FilePath
    , githubRunner :: FilePath -> [String] -> Maybe ByteString -> IO CommandResult
    }
