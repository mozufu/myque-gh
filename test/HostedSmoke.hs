{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Item (Kind (..), State (..), WorkItem (..), newWorkItem)
import Myque.Store (initLayout, saveItem)
import Myque.Timestamp (Timestamp, parseTimestamp)
import Myque.Uuid (Uuid, newUuidV7, uuidText)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Text.Read (readMaybe)

repo :: String
repo = "mozufu/myque-gh-smoke"

main :: IO ()
main = do
    arguments <- getArgs
    unless (arguments == ["--repo", repo, "--allow-write"]) $ do
        putStrLn "refusing hosted mutations: require --repo mozufu/myque-gh-smoke --allow-write"
        exitFailure
    author <- maybe "iceice666" id <$> lookupEnv "MYQUE_GH_ISSUE_AUTHOR"
    outcome <- try (runSmoke author)
    case outcome of
        Right () -> pure ()
        Left failure -> do
            putStrLn ("hosted smoke failed: " <> show (failure :: SomeException))
            exitFailure

runSmoke :: String -> IO ()
runSmoke author = withSystemTempDirectory "myque-gh-hosted-smoke" $ \root -> withSystemTempDirectory "myque-gh-empty-cache" $ \freshCache -> do
    layout <- initLayout root
    created <- timestamp "2026-08-20T14:21:00+08:00"
    closed <- timestamp "2026-08-26T19:42:00+08:00"
    firstUuid <- newUuidV7
    secondUuid <- newUuidV7
    terminalUuid <- newUuidV7
    milestoneUuid <- newUuidV7
    let milestoneTitle = "[myque-gh smoke] release " <> uuidText milestoneUuid
        milestone = newWorkItem milestoneUuid Milestone created milestoneTitle
        first = (newWorkItem firstUuid Task created "[myque-gh smoke] first"){itemParent = Just milestoneUuid}
        second = (newWorkItem secondUuid Bug created "[myque-gh smoke] second"){itemParent = Just firstUuid}
        terminal = (newWorkItem terminalUuid Task created "[myque-gh smoke] historical done"){itemState = Done, itemClosed = Just closed}
    mapM_ (saveItem layout) [milestone, first, second, terminal]
    _ <- git root ["init", "-q", "--initial-branch=main"]
    _ <- commit root "hosted smoke fixtures"
    firstPlan <- successfulCli root (projectionArgs author "plan" root)
    unless (uuidIn firstUuid firstPlan && uuidIn secondUuid firstPlan && not (uuidIn terminalUuid firstPlan)) (fail "plan did not select exactly the non-terminal smoke fixtures")
    _ <- successfulCli root (projectionArgs author "apply" root)
    firstIssue <- discoverSmokeIssue firstUuid
    secondIssue <- discoverSmokeIssue secondUuid
    milestoneIssue <- discoverSmokeIssue milestoneUuid
    milestoneNumber <- discoverSmokeMilestone milestoneUuid
    verifyMilestone milestoneNumber milestoneTitle "open"
    verifyMembership milestoneIssue Nothing
    verifyMembership firstIssue (Just milestoneNumber)
    verifyMembership secondIssue (Just milestoneNumber)
    verifyParent milestoneIssue firstIssue
    terminalIssue <- findSmokeIssue terminalUuid
    unless (terminalIssue == Nothing) (fail "first projection created a terminal-only smoke issue")
    verifyParent firstIssue secondIssue
    ensureHumanLabel
    _ <- ghSilent ["api", "--hostname", "github.com", "--method", "POST", "/repos/" <> repo <> "/issues/" <> show firstIssue <> "/labels", "-f", "labels[]=smoke:human"]
    _ <- ghSilent ["api", "--hostname", "github.com", "--method", "POST", "/repos/" <> repo <> "/issues/" <> show firstIssue <> "/comments", "-f", "body=myque-gh smoke: preserve this comment"]
    _ <- ghSilent ["api", "--hostname", "github.com", "--method", "PATCH", "/repos/" <> repo <> "/issues/" <> show firstIssue, "-f", "title=[myque-gh smoke] manually drifted", "-f", "state=closed"]
    _ <- successfulCliWithCache freshCache root (projectionArgs author "apply" root)
    noDrift <- successfulCliWithCache freshCache root (projectionArgs author "plan" root)
    unless ("No changes." `isInfixOf` noDrift) (fail ("fresh-cache hosted projection did not converge:\n" <> noDrift))
    verifyHumanFacts firstIssue
    let renamedTitle = milestoneTitle <> " renamed"
        renamed = milestone{itemBody = "# " <> renamedTitle <> "\n"}
    _ <- saveItem layout renamed
    _ <- commit root "rename hosted smoke milestone"
    _ <- successfulCli root (projectionArgs author "apply" root)
    renamedNumber <- discoverSmokeMilestone milestoneUuid
    unless (renamedNumber == milestoneNumber) (fail "milestone rename created a different native number")
    verifyMilestone milestoneNumber renamedTitle "open"
    verifyMembership firstIssue (Just milestoneNumber)
    verifyMembership secondIssue (Just milestoneNumber)
    mapM_
        (saveItem layout)
        [ renamed{itemState = Done, itemClosed = Just closed, itemUpdated = Just closed}
        , (cancel closed first){itemParent = Nothing}
        , (cancel closed second){itemParent = Nothing}
        , terminal
        ]
    _ <- commit root "cancel hosted smoke fixtures"
    _ <- successfulCli root (projectionArgs author "apply" root)
    verifyCancelled firstIssue
    verifyCancelled secondIssue
    verifyNoParent secondIssue
    verifyNoParent firstIssue
    verifyMembership firstIssue Nothing
    verifyMembership secondIssue Nothing
    verifyMilestone milestoneNumber renamedTitle "closed"
    verifyHumanFacts firstIssue
    withSystemTempDirectory "myque-gh-final-empty-cache" $ \finalCache -> do
        finalPlan <- successfulCliWithCache finalCache root (projectionArgs author "plan" root)
        unless ("No changes." `isInfixOf` finalPlan) (fail ("closed/detached milestone projection did not converge:\n" <> finalPlan))
    putStrLn (T.unpack (uuidText milestoneUuid) <> " -> https://github.com/" <> repo <> "/milestone/" <> show milestoneNumber)
    putStrLn (T.unpack (uuidText firstUuid) <> " -> https://github.com/" <> repo <> "/issues/" <> show firstIssue)
    putStrLn (T.unpack (uuidText secondUuid) <> " -> https://github.com/" <> repo <> "/issues/" <> show secondIssue)

projectionArgs :: String -> String -> FilePath -> [String]
projectionArgs author command root =
    [ command
    , "--store"
    , root
    , "--repo"
    , repo
    , "--ref"
    , "refs/heads/main"
    , "--issue-author"
    , author
    , "--project"
    , "kind = task or kind = bug or kind = issue"
    ]

cancel :: Timestamp -> WorkItem -> WorkItem
cancel closed item = item{itemState = Cancelled, itemClosed = Just closed, itemUpdated = Just closed}

timestamp :: Text -> IO Timestamp
timestamp raw = either fail pure (parseTimestamp raw)

uuidIn :: Uuid -> String -> Bool
uuidIn value text = T.unpack (uuidText value) `isInfixOf` text

commit :: FilePath -> String -> IO String
commit root message = do
    _ <- git root ["add", "."]
    git root ["-c", "user.name=Hosted Smoke", "-c", "user.email=smoke@example.test", "-c", "commit.gpgsign=false", "commit", "-m", message]

git :: FilePath -> [String] -> IO String
git root arguments = successful ((proc "git" ("-C" : root : arguments)){cwd = Just root}) ""

gh :: [String] -> IO String
gh arguments = successful (proc "gh" arguments) ""

ghSilent :: [String] -> IO String
ghSilent arguments = gh (arguments <> ["--silent"])

successfulCli :: FilePath -> [String] -> IO String
successfulCli root arguments = successful ((proc "myque-gh" arguments){cwd = Just root}) ""

successfulCliWithCache :: FilePath -> FilePath -> [String] -> IO String
successfulCliWithCache cache root arguments = do
    environment <- getEnvironment
    let process = (proc "myque-gh" arguments){cwd = Just root, env = Just (("XDG_CACHE_HOME", cache) : filter ((/= "XDG_CACHE_HOME") . fst) environment)}
    successful process ""

successful :: CreateProcess -> String -> IO String
successful process input = do
    (exitCode, out, err) <- readCreateProcessWithExitCode process input
    unless (exitCode == ExitSuccess) (fail (err <> out))
    pure out

findSmokeIssue :: Uuid -> IO (Maybe Int)
findSmokeIssue value = do
    rows <- gh ["api", "--hostname", "github.com", "--paginate", "/repos/" <> repo <> "/issues?state=all&per_page=100", "--jq", query]
    pure $ case filter (not . null) (lines rows) of
        [] -> Nothing
        row : _ -> readMaybe row
  where
    marker = "<!-- myque:id=" <> T.unpack (uuidText value) <> " -->"
    query = ".[] | select(((.body // \"\") | split(\"\\n\")[0]) == \"" <> marker <> "\") | .number"

discoverSmokeIssue :: Uuid -> IO Int
discoverSmokeIssue value = do
    found <- findSmokeIssue value
    case found of
        Just number -> pure number
        Nothing -> fail ("created issue not found for " <> T.unpack (uuidText value))

discoverSmokeMilestone :: Uuid -> IO Int
discoverSmokeMilestone value = do
    rows <- gh ["api", "--hostname", "github.com", "--paginate", "/repos/" <> repo <> "/milestones?state=all&per_page=100", "--jq", query]
    case filter (not . null) (lines rows) of
        [row] | Just number <- readMaybe row -> pure number
        _ -> fail ("expected exactly one native milestone for " <> T.unpack (uuidText value))
  where
    marker = "<!-- myque:id=" <> T.unpack (uuidText value) <> " -->"
    query = ".[] | select(((.description // \"\") | split(\"\\n\")[0]) == \"" <> marker <> "\") | .number"

verifyMembership :: Int -> Maybe Int -> IO ()
verifyMembership issue expected = do
    actual <- gh ["api", "--hostname", "github.com", "/repos/" <> repo <> "/issues/" <> show issue, "--jq", ".milestone.number // \"none\""]
    unless (trim actual == maybe "none" show expected) (fail ("unexpected native milestone membership on issue #" <> show issue))

verifyMilestone :: Int -> Text -> String -> IO ()
verifyMilestone number title expectedState = do
    actual <- gh ["api", "--hostname", "github.com", "/repos/" <> repo <> "/milestones/" <> show number, "--jq", "[.title,.state] | @tsv"]
    unless (trim actual == T.unpack title <> "\t" <> expectedState) (fail "native milestone title/state did not converge")

ensureHumanLabel :: IO ()
ensureHumanLabel = do
    _ <- gh ["label", "create", "smoke:human", "--repo", repo, "--color", "ededed", "--description", "Hosted smoke label.", "--force"]
    pure ()

verifyHumanFacts :: Int -> IO ()
verifyHumanFacts issue = do
    labelPresent <- gh ["api", "--hostname", "github.com", "/repos/" <> repo <> "/issues/" <> show issue, "--jq", "[.labels[].name] | index(\"smoke:human\") != null"]
    unless (trim labelPresent == "true") (fail "human label was not preserved")
    commentPresent <- gh ["api", "--hostname", "github.com", "/repos/" <> repo <> "/issues/" <> show issue <> "/comments", "--jq", "map(.body == \"myque-gh smoke: preserve this comment\") | any"]
    unless (trim commentPresent == "true") (fail "human comment was not preserved")

verifyParent :: Int -> Int -> IO ()
verifyParent parent child = do
    actual <- gh ["api", "--hostname", "github.com", "/repos/" <> repo <> "/issues/" <> show child <> "/parent", "--jq", ".number"]
    unless (trim actual == show parent) (fail "smoke child was not attached to its canonical parent")

verifyNoParent :: Int -> IO ()
verifyNoParent child = do
    (exitCode, _out, _err) <- readCreateProcessWithExitCode (proc "gh" ["api", "--hostname", "github.com", "/repos/" <> repo <> "/issues/" <> show child <> "/parent"]) ""
    unless (exitCode == ExitFailure 1) (fail "smoke child parent was not removed")

verifyCancelled :: Int -> IO ()
verifyCancelled issue = do
    state <- gh ["api", "--hostname", "github.com", "/repos/" <> repo <> "/issues/" <> show issue, "--jq", "[.state,.state_reason] | @tsv"]
    unless (trim state == "closed\tnot_planned") (fail "cancelled smoke issue was not closed/not_planned")

trim :: String -> String
trim = T.unpack . T.strip . T.pack
