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
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode (..), exitFailure)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Text.Read (readMaybe)

repo :: String
repo = "mozufu/myque-gh"

main :: IO ()
main = do
    arguments <- getArgs
    unless (arguments == ["--repo", repo, "--allow-write"]) $ do
        putStrLn "refusing hosted mutations: require --repo mozufu/myque-gh --allow-write"
        exitFailure
    outcome <- try runSmoke
    case outcome of
        Right () -> pure ()
        Left failure -> do
            putStrLn ("hosted smoke failed: " <> show (failure :: SomeException))
            exitFailure

runSmoke :: IO ()
runSmoke = withSystemTempDirectory "myque-gh-hosted-smoke" $ \root -> withSystemTempDirectory "myque-gh-empty-cache" $ \freshCache -> do
    layout <- initLayout root
    created <- timestamp "2026-08-20T14:21:00+08:00"
    closed <- timestamp "2026-08-26T19:42:00+08:00"
    firstUuid <- newUuidV7
    secondUuid <- newUuidV7
    terminalUuid <- newUuidV7
    let first = newWorkItem firstUuid Task created "[myque-gh smoke] first"
        second = newWorkItem secondUuid Bug created "[myque-gh smoke] second"
        terminal = (newWorkItem terminalUuid Task created "[myque-gh smoke] historical done"){itemState = Done, itemClosed = Just closed}
    mapM_ (saveItem layout) [first, second, terminal]
    _ <- git root ["init", "-q", "--initial-branch=main"]
    _ <- commit root "hosted smoke fixtures"
    firstPlan <- successfulCli root (projectionArgs "plan" root)
    unless (uuidIn firstUuid firstPlan && uuidIn secondUuid firstPlan && not (uuidIn terminalUuid firstPlan)) (fail "plan did not select exactly the non-terminal smoke fixtures")
    _ <- successfulCli root (projectionArgs "apply" root)
    firstIssue <- discoverSmokeIssue firstUuid
    secondIssue <- discoverSmokeIssue secondUuid
    terminalIssue <- findSmokeIssue terminalUuid
    unless (terminalIssue == Nothing) (fail "first projection created a terminal-only smoke issue")
    ensureHumanLabel
    _ <- ghSilent ["api", "--hostname", "github.com", "--method", "POST", "/repos/" <> repo <> "/issues/" <> show firstIssue <> "/labels", "-f", "labels[]=smoke:human"]
    _ <- ghSilent ["api", "--hostname", "github.com", "--method", "POST", "/repos/" <> repo <> "/issues/" <> show firstIssue <> "/comments", "-f", "body=myque-gh smoke: preserve this comment"]
    _ <- ghSilent ["api", "--hostname", "github.com", "--method", "PATCH", "/repos/" <> repo <> "/issues/" <> show firstIssue, "-f", "title=[myque-gh smoke] manually drifted", "-f", "state=closed"]
    _ <- successfulCliWithCache freshCache root (projectionArgs "apply" root)
    noDrift <- successfulCliWithCache freshCache root (projectionArgs "plan" root)
    unless ("No changes." `isInfixOf` noDrift) (fail ("fresh-cache hosted projection did not converge:\n" <> noDrift))
    verifyHumanFacts firstIssue
    mapM_ (saveItem layout) [cancel closed first, cancel closed second, terminal]
    _ <- commit root "cancel hosted smoke fixtures"
    _ <- successfulCli root (projectionArgs "apply" root)
    verifyCancelled firstIssue
    verifyCancelled secondIssue
    putStrLn (T.unpack (uuidText firstUuid) <> " -> https://github.com/" <> repo <> "/issues/" <> show firstIssue)
    putStrLn (T.unpack (uuidText secondUuid) <> " -> https://github.com/" <> repo <> "/issues/" <> show secondIssue)

projectionArgs :: String -> FilePath -> [String]
projectionArgs command root =
    [ command
    , "--store"
    , root
    , "--repo"
    , repo
    , "--ref"
    , "refs/heads/main"
    , "--issue-author"
    , "iceice666"
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

verifyCancelled :: Int -> IO ()
verifyCancelled issue = do
    state <- gh ["api", "--hostname", "github.com", "/repos/" <> repo <> "/issues/" <> show issue, "--jq", "[.state,.state_reason] | @tsv"]
    unless (trim state == "closed\tnot_planned") (fail "cancelled smoke issue was not closed/not_planned")

trim :: String -> String
trim = T.unpack . T.strip . T.pack
