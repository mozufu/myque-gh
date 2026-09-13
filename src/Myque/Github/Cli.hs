{-# LANGUAGE OverloadedStrings #-}

module Myque.Github.Cli (runCli) where

import Control.Exception (catch, throwIO)
import Control.Monad (unless, when)
import Data.Aeson (object, (.=))
import Data.Char (isAlphaNum, isControl)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Myque.Github.Github
import Myque.Github.Markers
import Myque.Github.Reconcile
import Myque.Github.Source
import Myque.Github.Types hiding (Failure)
import Myque.Github.Types qualified as Types
import Myque.Item (WorkItem (itemId))
import Myque.Store (parseSelector, resolveSelector)
import Options.Applicative hiding (failureCode)
import System.Exit (ExitCode (..))
import System.IO (stderr)

data Command
    = PlanCommand Common
    | ApplyCommand Common
    | PrLinkCommand PrLink

data Common = Common
    { commonStore :: FilePath
    , commonRepo :: String
    , commonAuthors :: [String]
    , commonRef :: String
    , commonSourceRepo :: Maybe String
    , commonSourceBranch :: Maybe String
    }

data PrLink = PrLink
    { prNumber :: Int
    , prItems :: [String]
    , prStore :: FilePath
    , prRepo :: String
    , prRef :: String
    , prApply :: Bool
    , prClear :: Bool
    }

runCli :: [String] -> IO ExitCode
runCli arguments = handleFailure $ case execParserPure defaultPrefs parserInfo arguments of
    Success parsedCommand -> runCommand parsedCommand
    Failure failure -> do
        let (message, parserExit) = renderFailure failure "myque-gh"
        if parserExit == ExitSuccess then putStr message else TIO.hPutStrLn stderr (T.pack message)
        pure parserExit
    CompletionInvoked completion -> do
        message <- execCompletion completion "myque-gh"
        putStr message
        pure ExitSuccess
  where
    handleFailure operation =
        operation `catch` \failure -> do
            TIO.hPutStrLn stderr (failureDiagnostic failure)
            pure (failureExitCode failure)

runCommand :: Command -> IO ExitCode
runCommand parsedCommand = case parsedCommand of
    PlanCommand common -> runProjection False common
    ApplyCommand common -> runProjection True common
    PrLinkCommand options -> runPrLink options

runProjection :: Bool -> Common -> IO ExitCode
runProjection applying common = do
    target <- parseTarget (commonRepo common) (commonAuthors common) applying
    source <- parseSource common applying
    withSnapshot source $ \snapshot -> do
        if applying
            then do
                plan <- reconcile defaultGithubClient source target snapshot
                mapM_ printWarning (planWarnings plan)
                TIO.putStr (renderPlan target snapshot plan)
                pure ExitSuccess
            else do
                github <- discoverGithub defaultGithubClient target snapshot
                plan <- either throwConflicts pure (buildPlan target snapshot github)
                mapM_ printWarning (planWarnings plan)
                TIO.putStr (renderPlan target snapshot plan)
                pure ExitSuccess

runPrLink :: PrLink -> IO ExitCode
runPrLink options = do
    target <- parseTarget (prRepo options) [] False
    when (prNumber options <= 0) (usage "PR number must be positive")
    when (prClear options && not (null (prItems options))) (usage "--clear and ITEM are mutually exclusive")
    when (not (prClear options) && null (prItems options)) (usage "pr link requires ITEM... or --clear")
    let source = SourceSpec (prStore options) (T.pack (prRef options)) Nothing Nothing Nothing
    withSnapshot source $ \snapshot -> do
        desired <-
            if prClear options
                then pure Set.empty
                else Set.fromList <$> traverse (resolve snapshot) (prItems options)
        (body, baseRepo) <- getPullRequest defaultGithubClient target (prNumber options)
        unless (T.toCaseFold baseRepo == T.toCaseFold (targetOwner target <> "/" <> targetRepo target)) (config "pull request base repository does not match --repo")
        rewritten <- either (\(MarkerError message) -> config message) pure (rewritePrLinks body desired)
        if rewritten == body
            then TIO.putStrLn "No changes." >> pure ExitSuccess
            else
                if not (prApply options)
                    then do
                        TIO.putStrLn ("~ PR #" <> T.pack (show (prNumber options)) <> " body trailer")
                        pure ExitSuccess
                    else do
                        (latest, _) <- getPullRequest defaultGithubClient target (prNumber options)
                        unless (latest == body) (remote "pr-body-changed")
                        currentSha <- resolveSourceRef (sourceRoot source) (sourceRef source)
                        unless (currentSha == snapshotSha snapshot) (remote "source-ref-moved")
                        _ <- restSingle defaultGithubClient "PATCH" (repoPath target <> "/pulls/" <> T.pack (show (prNumber options))) (Just (object ["body" .= rewritten]))
                        TIO.putStrLn ("Updated PR #" <> T.pack (show (prNumber options)) <> " myque links.")
                        pure ExitSuccess
  where
    resolve snapshot raw = do
        selector <- either (config . T.pack) pure (parseSelector (T.pack raw))
        item <- either (config . T.pack) pure (resolveSelector (snapshotStore snapshot) selector)
        pure (itemId item)

parseTarget :: String -> [String] -> Bool -> IO Target
parseTarget raw authors requireAuthors = do
    (owner, repo) <- either config pure (parseRepo (T.pack raw))
    when (requireAuthors && null authors) (usage "plan/apply require at least one --issue-author")
    let normalizedAuthors = Set.fromList (map (T.toCaseFold . T.strip . T.pack) authors)
    when (any T.null (Set.toList normalizedAuthors)) (usage "--issue-author must not be empty")
    pure Target{targetOwner = T.toLower owner, targetRepo = T.toLower repo, targetIssueAuthors = normalizedAuthors}

parseSource :: Common -> Bool -> IO SourceSpec
parseSource common applying = do
    sourceLink <- case (commonSourceRepo common, commonSourceBranch common) of
        (Nothing, Nothing) -> pure (Nothing, Nothing)
        (Just rawRepo, Just branch) -> do
            repo <- either config pure (parseRepo (T.pack rawRepo))
            validateBranch (commonStore common) (T.pack branch)
            pure (Just repo, Just (T.pack branch))
        _ -> usage "--source-repo and --source-branch must be supplied together"
    let ref = T.pack (commonRef common)
    when applying (validatePublicationRef (commonStore common) ref)
    pure
        SourceSpec
            { sourceRoot = commonStore common
            , sourceRef = ref
            , sourcePublicationRef = if applying then Just ref else Nothing
            , sourceLinkRepo = fst sourceLink
            , sourceLinkBranch = snd sourceLink
            }

validateBranch :: FilePath -> Text -> IO ()
validateBranch root branch = do
    when (T.null branch) (usage "--source-branch must not be empty")
    validatePublicationRef root ("refs/heads/" <> branch)

parseRepo :: Text -> Either Text (Text, Text)
parseRepo raw = case T.splitOn "/" raw of
    [owner, repo]
        | validOwner owner && validRepo repo -> Right (owner, repo)
    _ -> Left "repository must be OWNER/REPO using GitHub name syntax"
  where
    validOwner owner = not (T.null owner) && isAlphaNum (T.head owner) && T.all (\char -> isAlphaNum char || char == '-') owner && not (T.any isControl owner)
    validRepo repo = not (T.null repo) && repo `notElem` [".", ".."] && T.all (\char -> isAlphaNum char || char `elem` ("_.-" :: String)) repo && not (T.any isControl repo)

parserInfo :: ParserInfo Command
parserInfo = info (helper <*> commandParser) (fullDesc <> header "myque-gh projects canonical myque items into GitHub Issues" <> footer ownership)
  where
    ownership = "Canonical myque state owns completion. GitHub comments/reviews remain native facts. Identity lives only in trusted issue headers and PR trailers; removing them loses the association."

commandParser :: Parser Command
commandParser =
    hsubparser
        ( command "plan" (info (PlanCommand <$> commonParser False) (progDesc "Read committed canonical state and print GitHub drift without mutation"))
            <> command "apply" (info (ApplyCommand <$> commonParser True) (progDesc "Converge managed Issue fields with guarded mutations"))
            <> command "pr" (info prParser (progDesc "Manage pull request work-item links"))
        )

prParser :: Parser Command
prParser = hsubparser (command "link" (info (PrLinkCommand <$> prLinkParser) (progDesc "Replace or clear the machine PR link trailer")))

commonParser :: Bool -> Parser Common
commonParser applying =
    Common
        <$> strOption (long "store" <> metavar "PATH" <> help "Git repository root containing canonical .tasks")
        <*> strOption (long "repo" <> metavar "OWNER/REPO" <> help "Exact github.com target")
        <*> many (strOption (long "issue-author" <> metavar "LOGIN" <> help "Trusted projection creator; repeatable"))
        <*> strOption (long "ref" <> metavar "REF" <> value (if applying then "" else "HEAD") <> showDefault <> help "Committed source ref; apply requires refs/heads/BRANCH")
        <*> optional (strOption (long "source-repo" <> metavar "OWNER/REPO" <> help "Repository used only for canonical hyperlinks"))
        <*> optional (strOption (long "source-branch" <> metavar "BRANCH" <> help "Branch used only for canonical hyperlinks"))

prLinkParser :: Parser PrLink
prLinkParser =
    PrLink
        <$> argument auto (metavar "NUMBER")
        <*> many (argument str (metavar "ITEM..."))
        <*> strOption (long "store" <> metavar "PATH")
        <*> strOption (long "repo" <> metavar "OWNER/REPO")
        <*> strOption (long "ref" <> metavar "REF" <> value "HEAD" <> showDefault)
        <*> switch (long "apply" <> help "Write the PR trailer")
        <*> switch (long "clear" <> help "Remove the managed trailer")

printWarning :: Warning -> IO ()
printWarning warning = TIO.hPutStrLn stderr ("warning: " <> warningCode warning <> " " <> warningDetail warning)

throwConflicts :: [Conflict] -> IO a
throwConflicts values = config (T.intercalate "\n" [conflictCode conflict <> ": " <> conflictDetail conflict | conflict <- values])

usage :: Text -> IO a
usage message = throwIO Types.Failure{failureCode = 2, failureDiagnostic = message}

config :: Text -> IO a
config message = throwIO Types.Failure{failureCode = 1, failureDiagnostic = message}

remote :: Text -> IO a
remote message = throwIO Types.Failure{failureCode = 3, failureDiagnostic = message}
