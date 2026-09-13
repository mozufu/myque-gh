{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Myque.Github.Types (
    Target (..),
    SourceSpec (..),
    Snapshot (..),
    Failure (..),
    failureExitCode,
    IssueState (..),
    CloseReason (..),
    GithubLabel (..),
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
import GHC.Generics (Generic)
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

failureExitCode :: Failure -> ExitCode
failureExitCode failure = ExitFailure (failureCode failure)

data IssueState = IssueOpen | IssueClosed
    deriving (Eq, Ord, Show, Generic)

data CloseReason = Completed | NotPlanned
    deriving (Eq, Ord, Show, Generic)

data GithubLabel = GithubLabel
    { githubLabelName :: Text
    , githubLabelColor :: Text
    , githubLabelDescription :: Maybe Text
    }
    deriving (Eq, Show, Generic)

data GithubIssue = GithubIssue
    { githubIssueNumber :: Int
    , githubIssueNodeId :: Text
    , githubIssueAuthor :: Text
    , githubIssueBody :: Text
    , githubIssueTitle :: Text
    , githubIssueState :: IssueState
    , githubIssueReason :: Maybe CloseReason
    , githubIssueLabels :: Set Text
    , githubIssueUpdatedAt :: Text
    , githubIssueUrl :: Maybe Text
    }
    deriving (Eq, Show, Generic)

data RepositoryMeta = RepositoryMeta
    { repositoryId :: Integer
    , repositoryNameWithOwner :: Text
    , repositoryArchived :: Bool
    , repositoryHasIssues :: Bool
    }
    deriving (Eq, Show, Generic)

data PrLifecycle = PrOpen | PrMerged | PrClosedUnmerged
    deriving (Eq, Ord, Show, Generic)

data CiState = CiPassing | CiFailing | CiPending | CiNone | CiUnknown
    deriving (Eq, Ord, Show, Generic)

data ReviewState = ReviewApproved | ReviewChangesRequested | ReviewRequired | ReviewNone | ReviewUnknown
    deriving (Eq, Ord, Show, Generic)

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
    deriving (Eq, Show, Generic)

data GithubSnapshot = GithubSnapshot
    { githubRepository :: RepositoryMeta
    , githubIssues :: [GithubIssue]
    , githubLabels :: [GithubLabel]
    , githubIssueByUuid :: Map Uuid GithubIssue
    , githubPrsByUuid :: Map Uuid [LinkedPr]
    , githubWarnings :: [Warning]
    }
    deriving (Eq, Show)

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

data IssueChange
    = SetTitle Text
    | SetBody Text
    | SetState IssueState (Maybe CloseReason)
    | AddLabel Text
    | RemoveLabel Text
    deriving (Eq, Show)

data Conflict = Conflict
    { conflictCode :: Text
    , conflictDetail :: Text
    }
    deriving (Eq, Ord, Show)

data Warning = Warning
    { warningCode :: Text
    , warningDetail :: Text
    }
    deriving (Eq, Ord, Show)

data Plan = Plan
    { planLabelCreates :: [Text]
    , planIssueCreates :: Map Uuid DesiredIssue
    , planIssueChanges :: Map Int (Uuid, Text, [IssueChange])
    , planWarnings :: [Warning]
    }
    deriving (Eq, Show)

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
