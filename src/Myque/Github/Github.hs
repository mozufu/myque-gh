{-# LANGUAGE OverloadedStrings #-}

-- | GitHub CLI transport, REST discovery, and pull-request observations.
module Myque.Github.Github (
    defaultGithubClient,
    discoverGithub,
    discoverCore,
    getRepository,
    getIssue,
    getPullRequest,
    restSingle,
    restPaginated,
    graphqlPaginated,
    encodePathSegment,
    repoPath,
    parseIssue,
) where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (forM, unless, when)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum, ord)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Myque.Github.Markers
import Myque.Github.Types
import Myque.Store (Store (..))
import Myque.Uuid (Uuid)
import Numeric (showHex)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process.Typed (
    byteStringInput,
    proc,
    readProcess,
    setEnv,
    setStdin,
 )

apiHeaders :: [String]
apiHeaders = ["-H", "Accept: application/vnd.github+json", "-H", "X-GitHub-Api-Version: 2026-03-10"]

-- | Production GitHub CLI client with prompting, paging, and debug output disabled.
defaultGithubClient :: GithubClient
defaultGithubClient = GithubClient "gh" run
  where
    run executable args stdinBytes = do
        environment <- sanitizeEnvironment <$> getEnvironment
        let configured = setEnv environment $ maybe id (setStdin . byteStringInput . BL.fromStrict) stdinBytes (proc executable args)
        (exitCode, stdoutBytes, stderrBytes) <- readProcess configured
        pure CommandResult{commandExit = exitCode, commandStdout = BL.toStrict stdoutBytes, commandStderr = BL.toStrict stderrBytes}
            `catch` handleMissing
    handleMissing :: IOException -> IO CommandResult
    handleMissing err = throwIO Failure{failureCode = 2, failureDiagnostic = "cannot execute gh; install GitHub CLI and authenticate with gh auth login or GH_TOKEN/GITHUB_TOKEN: " <> T.pack (show err)}

sanitizeEnvironment :: [(String, String)] -> [(String, String)]
sanitizeEnvironment environment =
    [("GH_PROMPT_DISABLED", "1"), ("GH_PAGER", "cat"), ("NO_COLOR", "1")]
        <> filter (\(name, _) -> name `notElem` ["GH_PROMPT_DISABLED", "GH_PAGER", "NO_COLOR", "GH_DEBUG"]) environment

-- | Discover issues, labels, pull-request links, and optional head facts.
discoverGithub :: GithubClient -> Target -> Snapshot -> IO GithubSnapshot
discoverGithub client target snapshot = do
    core <- discoverCore client target snapshot
    enrichPullRequestFacts client target core

-- | Discover repository, issue, label, and pull-request identity data without optional facts.
discoverCore :: GithubClient -> Target -> Snapshot -> IO GithubSnapshot
discoverCore client target snapshot = do
    repository <- getRepository client target
    issuePages <- restPaginated client (repoPath target <> "/issues?state=all&sort=created&direction=asc&per_page=100")
    labelPages <- restPaginated client (repoPath target <> "/labels?per_page=100")
    prPages <- graphqlPaginated client corePullRequestsQuery target
    issues <- parseIssuePages issuePages
    labels <- parsePages "labels" parseLabel labelPages
    prs <- parsePullRequestPages prPages
    (issueMap, issueWarnings) <- identifyIssues target issues
    let canonical = storeById (snapshotStore snapshot)
        (prMap, prWarnings) = identifyPullRequests canonical target prs
    pure
        GithubSnapshot
            { githubRepository = repository
            , githubIssues = issues
            , githubLabels = labels
            , githubIssueByUuid = issueMap
            , githubPrsByUuid = prMap
            , githubWarnings = issueWarnings <> prWarnings
            }

-- | Read and validate target repository identity and issue availability.
getRepository :: GithubClient -> Target -> IO RepositoryMeta
getRepository client target = do
    (_, value) <- restSingle client "GET" (repoPath target) Nothing
    repository <- decodeValue "repository metadata" parseRepository value
    let expected = T.toCaseFold (targetOwner target <> "/" <> targetRepo target)
    when (T.toCaseFold (repositoryNameWithOwner repository) /= expected) (remoteFailure 1 "target-mismatch: repository resolved to another nameWithOwner")
    when (repositoryArchived repository) (remoteFailure 1 "target repository is archived")
    unless (repositoryHasIssues repository) (remoteFailure 1 "target repository has Issues disabled")
    pure repository

-- | Read one issue and all fields used by reconciliation.
getIssue :: GithubClient -> Target -> Int -> IO GithubIssue
getIssue client target number = do
    (_, value) <- restSingle client "GET" (repoPath target <> "/issues/" <> T.pack (show number)) Nothing
    decodeValue "issue" parseIssue value

-- | Read a pull-request body and exact base repository name.
getPullRequest :: GithubClient -> Target -> Int -> IO (Text, Text)
getPullRequest client target number = do
    (_, value) <- restSingle client "GET" (repoPath target <> "/pulls/" <> T.pack (show number)) Nothing
    decodeValue "pull request" parser value
  where
    parser = withObject "pull request" $ \row -> do
        body <- row .:? "body" .!= ""
        base <- row .: "base" >>= withObject "base" (.: "repo") >>= withObject "repo" (.: "full_name")
        pure (body, base)

-- | Invoke a paginated REST endpoint and return its page values.
restPaginated :: GithubClient -> Text -> IO [Value]
restPaginated client endpoint = do
    result <- githubRunner client (githubExecutable client) (["api", "--hostname", "github.com", "--paginate", "--slurp", T.unpack endpoint] <> apiHeaders) Nothing
    unless (commandExit result == ExitSuccess) (transportFailure result)
    value <- decodeJson "paginated REST response" (commandStdout result)
    case value of
        Array pages -> pure (foldr (:) [] pages)
        _ -> remoteFailure 3 "paginated REST response is not a JSON array of pages"

-- | Invoke a paginated GraphQL query with target owner and repository variables.
graphqlPaginated :: GithubClient -> Text -> Target -> IO [Value]
graphqlPaginated client query target = do
    let args = ["api", "--hostname", "github.com", "graphql", "--paginate", "--slurp", "-f", "query=" <> T.unpack query, "-f", "owner=" <> T.unpack (targetOwner target), "-f", "name=" <> T.unpack (targetRepo target)]
    result <- githubRunner client (githubExecutable client) args Nothing
    unless (commandExit result == ExitSuccess) (transportFailure result)
    value <- decodeJson "paginated GraphQL response" (commandStdout result)
    case value of
        Array pages -> pure (foldr (:) [] pages)
        _ -> remoteFailure 3 "paginated GraphQL response is not a JSON array"

-- | Invoke one REST request using JSON stdin and parse the included HTTP response.
restSingle :: GithubClient -> Text -> Text -> Maybe Value -> IO (Int, Value)
restSingle client method endpoint payload = do
    let args = ["api", "--hostname", "github.com", "--method", T.unpack method, "--include", T.unpack endpoint] <> apiHeaders <> maybe [] (const ["--input", "-"]) payload
        input = BL.toStrict . encode <$> payload
    result <- githubRunner client (githubExecutable client) args input
    case parseIncludedResponse (commandStdout result) of
        Left message
            | commandExit result == ExitSuccess -> remoteFailure 3 message
            | otherwise -> transportFailure result
        Right (status, body)
            | status == 204 && BS.null body -> pure (status, Null)
            | otherwise -> do
                value <- decodeJson "single GitHub response" body
                if commandExit result == ExitSuccess && status >= 200 && status < 300
                    then pure (status, value)
                    else remoteFailure 3 (apiError status value)

parseIncludedResponse :: ByteString -> Either Text (Int, ByteString)
parseIncludedResponse raw = do
    let normalized = B8.pack (normalizeCrLf (B8.unpack raw))
        (headers, bodyWithGap) = B8.breakSubstring "\n\n" normalized
    body <- maybe (Left "transport-format: response lacks HTTP header separator") Right (BS.stripPrefix "\n\n" bodyWithGap)
    statusLine <- case B8.lines headers of
        first : _ -> Right first
        [] -> Left "transport-format: response lacks HTTP status"
    status <- case B8.words statusLine of
        (_ : code : _) -> case B8.readInt code of
            Just (number, trailing) | BS.null trailing -> Right number
            _ -> Left "transport-format: invalid HTTP status"
        _ -> Left "transport-format: invalid HTTP status line"
    whenEither ("HTTP/" `BS.isPrefixOf` body) "transport-format: unexpected second HTTP response"
    pure (status, body)

normalizeCrLf :: String -> String
normalizeCrLf [] = []
normalizeCrLf ('\r' : '\n' : rest) = '\n' : normalizeCrLf rest
normalizeCrLf (char : rest) = char : normalizeCrLf rest

parsePages :: Text -> (Value -> Parser a) -> [Value] -> IO [a]
parsePages name parser pages = do
    rows <- fmap concat $ forM pages $ \page -> case page of
        Array values -> traverse (decodeValue name parser) (foldr (:) [] values)
        _ -> remoteFailure 3 (name <> " page is not a JSON array")
    deduplicate name rows
  where
    deduplicate _ values = pure values

parseIssuePages :: [Value] -> IO [GithubIssue]
parseIssuePages pages = fmap concat $ forM pages $ \page -> case page of
    Array values -> fmap catMaybes $ forM (foldr (:) [] values) $ \value -> case value of
        Object row | KM.member "pull_request" row -> pure Nothing
        _ -> Just <$> decodeValue "issues" parseIssue value
    _ -> remoteFailure 3 "issues page is not a JSON array"

parseRepository :: Value -> Parser RepositoryMeta
parseRepository = withObject "repository" $ \row ->
    RepositoryMeta
        <$> row .: "id"
        <*> row .: "full_name"
        <*> row .: "archived"
        <*> row .: "has_issues"

-- | Parse an issue returned by GitHub's REST API.
parseIssue :: Value -> Parser GithubIssue
parseIssue = withObject "issue" $ \row -> do
    when (KM.member "pull_request" row) (fail "pull request row")
    labels <- row .: "labels" >>= traverse (withObject "label" (.: "name"))
    stateText' <- row .: "state"
    reasonText <- row .:? "state_reason"
    state <- parseIssueState stateText'
    reason <- if state == IssueOpen then pure Nothing else traverse parseCloseReason reasonText
    author <- row .: "user" >>= withObject "user" (.: "login")
    issueId <- row .: "id"
    number <- row .: "number"
    nodeId <- row .: "node_id"
    body <- row .:? "body" .!= ""
    title <- row .: "title"
    updatedAt <- row .: "updated_at"
    issueUrl <- row .:? "html_url"
    parentUrl <- row .:? "parent_issue_url"
    parent <- traverse parseIssueRef parentUrl
    pure (GithubIssue issueId number nodeId author body title state reason (Set.fromList labels) updatedAt issueUrl parent)

parseIssueRef :: Text -> Parser GithubIssueRef
parseIssueRef raw = case reverse (T.splitOn "/" raw) of
    numberText : "issues" : repo : owner : _ -> case decimal numberText of
        Just number -> pure (GithubIssueRef (T.toCaseFold owner) (T.toCaseFold repo) number)
        Nothing -> fail "parent issue URL has an invalid number"
    _ -> fail "parent issue URL has an invalid shape"
  where
    decimal text = case reads (T.unpack text) of
        [(number, "")] | number > 0 -> Just number
        _ -> Nothing

parseIssueState :: Text -> Parser IssueState
parseIssueState "open" = pure IssueOpen
parseIssueState "closed" = pure IssueClosed
parseIssueState other = fail ("unknown issue state: " <> T.unpack other)

parseCloseReason :: Text -> Parser CloseReason
parseCloseReason "completed" = pure Completed
parseCloseReason "not_planned" = pure NotPlanned
parseCloseReason "reopened" = fail "reopened close reason on closed issue"
parseCloseReason other = fail ("unknown close reason: " <> T.unpack other)

parseLabel :: Value -> Parser GithubLabel
parseLabel = withObject "label" $ \row -> GithubLabel <$> row .: "name" <*> row .: "color" <*> row .:? "description"

identifyIssues :: Target -> [GithubIssue] -> IO (Map Uuid GithubIssue, [Warning])
identifyIssues target issues = do
    claims <- fmap catMaybes $ forM issues $ \issue -> case parseIssueIdentity (githubIssueBody issue) of
        Left (MarkerError message)
            | trusted issue -> remoteFailure 1 ("identity-conflict issue #" <> number issue <> ": " <> message)
            | otherwise -> pure Nothing
        Right Nothing -> pure Nothing
        Right (Just uuid)
            | trusted issue -> pure (Just (uuid, issue, Nothing))
            | otherwise -> pure (Just (uuid, issue, Just Warning{warningCode = "untrusted-identity-claim", warningDetail = "issue #" <> number issue <> " by " <> githubIssueAuthor issue <> " claims " <> T.pack (show uuid)}))
    let trustedClaims = [(uuid, issue) | (uuid, issue, Nothing) <- claims]
        grouped = Map.fromListWith (<>) [(uuid, [issue]) | (uuid, issue) <- trustedClaims]
    case [(uuid, map githubIssueNumber rows) | (uuid, rows) <- Map.toList grouped, length rows > 1] of
        [] -> pure (Map.mapMaybe listToMaybe grouped, mapMaybe third claims)
        conflicts -> remoteFailure 1 ("duplicate projection identities: " <> T.intercalate "; " [T.pack (show uuid) <> " in " <> T.pack (show numbers) | (uuid, numbers) <- conflicts])
  where
    trusted issue = T.toCaseFold (githubIssueAuthor issue) `Set.member` targetIssueAuthors target
    number issue = T.pack (show (githubIssueNumber issue))
    third (_, _, value) = value

corePullRequestsQuery :: Text
corePullRequestsQuery = "query($owner:String!,$name:String!,$endCursor:String){repository(owner:$owner,name:$name){pullRequests(first:100,after:$endCursor,states:[OPEN,CLOSED,MERGED],orderBy:{field:CREATED_AT,direction:ASC}){nodes{number title body state isDraft headRefName headRefOid baseRefName baseRefOid headRepository{nameWithOwner} baseRepository{nameWithOwner} updatedAt}pageInfo{hasNextPage endCursor}}}}"

data CorePr = CorePr Int Text Text Text Bool (Maybe Text) Text Text Text Text

parsePullRequestPages :: [Value] -> IO [CorePr]
parsePullRequestPages pages = fmap concat $ forM pages $ \page -> decodeValue "pull request page" parser page
  where
    parser = withObject "graphql page" $ \root -> do
        checkGraphqlErrors root
        repository <- root .: "data" >>= withObject "data" (.: "repository")
        connection <- withObject "repository" (.: "pullRequests") repository
        nodes <- withObject "connection" (.: "nodes") connection
        traverse parseCorePr nodes

parseCorePr :: Value -> Parser CorePr
parseCorePr = withObject "pull request" $ \row -> do
    headRepository <- row .:? "headRepository" >>= traverse (withObject "head repo" (.: "nameWithOwner"))
    baseRepository <- row .: "baseRepository" >>= withObject "base repo" (.: "nameWithOwner")
    CorePr <$> row .: "number" <*> row .: "title" <*> (row .:? "body" .!= "") <*> row .: "state" <*> row .: "isDraft" <*> pure headRepository <*> row .: "headRefName" <*> row .: "headRefOid" <*> pure baseRepository <*> row .: "baseRefName"

identifyPullRequests :: Map Uuid a -> Target -> [CorePr] -> (Map Uuid [LinkedPr], [Warning])
identifyPullRequests canonical _target prs = foldr add (Map.empty, []) prs
  where
    add pr@(CorePr number _ body _ _ _ _ _ _ _) (mapping, warnings) =
        let parsed = parsePrLinks body
         in if not (null (parsedPrDiagnostics parsed))
                then (mapping, Warning "malformed-pr-links" ("PR #" <> T.pack (show number) <> ": " <> T.intercalate "; " (parsedPrDiagnostics parsed)) : warnings)
                else foldr (insertOne pr) (mapping, warnings) (Set.toAscList (parsedPrUuids parsed))
    insertOne pr uuid (mapping, warnings)
        | Map.member uuid canonical = (Map.insertWith merge uuid [toLinked pr] mapping, warnings)
        | otherwise = (mapping, Warning "unknown-pr-link" ("PR #" <> T.pack (show (coreNumber pr)) <> " links unknown " <> T.pack (show uuid)) : warnings)
    merge new old = sortOn linkedPrNumber (dedup (new <> old))
    dedup = Map.elems . Map.fromList . map (\pr -> (linkedPrNumber pr, pr))

toLinked :: CorePr -> LinkedPr
toLinked (CorePr number title _ state draft headRepo headRef headSha baseRepo baseRef) =
    LinkedPr number title lifecycle draft headRepo headRef headSha baseRepo baseRef CiUnknown ReviewUnknown
  where
    lifecycle = case state of
        "OPEN" -> PrOpen
        "MERGED" -> PrMerged
        _ -> PrClosedUnmerged

coreNumber :: CorePr -> Int
coreNumber (CorePr number _ _ _ _ _ _ _ _ _) = number

enrichPullRequestFacts :: GithubClient -> Target -> GithubSnapshot -> IO GithubSnapshot
enrichPullRequestFacts client target snapshot = do
    let numbers = Map.keys (Map.fromList [(linkedPrNumber pr, ()) | prs <- Map.elems (githubPrsByUuid snapshot), pr <- prs])
    facts <- fmap Map.unions (traverse (queryFactBatch client target) (chunksOf 25 numbers))
    let update pr = case Map.lookup (linkedPrNumber pr) facts of
            Just (sha, ci, review) | sha == linkedPrHeadSha pr -> pr{linkedPrCi = ci, linkedPrReview = review}
            _ -> pr{linkedPrCi = CiUnknown, linkedPrReview = ReviewUnknown}
    pure snapshot{githubPrsByUuid = Map.map (map update) (githubPrsByUuid snapshot)}

queryFactBatch :: GithubClient -> Target -> [Int] -> IO (Map Int (Text, CiState, ReviewState))
queryFactBatch _ _ [] = pure Map.empty
queryFactBatch client target numbers = do
    let aliases = T.concat ["p" <> T.pack (show number) <> ":pullRequest(number:" <> T.pack (show number) <> "){headRefOid reviewDecision statusCheckRollup{state}}" | number <- numbers]
        query = "query($owner:String!,$name:String!){repository(owner:$owner,name:$name){" <> aliases <> "}}"
        args = ["api", "--hostname", "github.com", "graphql", "-f", "query=" <> T.unpack query, "-f", "owner=" <> T.unpack (targetOwner target), "-f", "name=" <> T.unpack (targetRepo target)]
    result <- githubRunner client (githubExecutable client) args Nothing
    if commandExit result /= ExitSuccess
        then pure Map.empty
        else case eitherDecodeStrict' (commandStdout result) >>= parseEither (parseFacts numbers) of
            Left _ -> pure Map.empty
            Right facts -> pure facts

parseFacts :: [Int] -> Value -> Parser (Map Int (Text, CiState, ReviewState))
parseFacts numbers = withObject "facts" $ \root -> do
    checkGraphqlErrors root
    repository <- root .: "data" >>= withObject "data" (.: "repository")
    fmap Map.fromList . fmap catMaybes $ forM numbers $ \number -> do
        value <- withObject "repository" (.:? Key.fromText ("p" <> T.pack (show number))) repository
        traverse
            ( withObject
                "fact"
                ( \row -> do
                    sha <- row .: "headRefOid"
                    review <- parseReview <$> row .:? "reviewDecision"
                    rollup <- row .:? "statusCheckRollup"
                    ci <- maybe (pure CiNone) (withObject "rollup" (fmap parseCi . (.: "state"))) rollup
                    pure (number, (sha, ci, review))
                )
            )
            value

parseCi :: Text -> CiState
parseCi "SUCCESS" = CiPassing
parseCi "FAILURE" = CiFailing
parseCi "ERROR" = CiFailing
parseCi "EXPECTED" = CiPending
parseCi "PENDING" = CiPending
parseCi _ = CiUnknown

parseReview :: Maybe Text -> ReviewState
parseReview (Just "APPROVED") = ReviewApproved
parseReview (Just "CHANGES_REQUESTED") = ReviewChangesRequested
parseReview (Just "REVIEW_REQUIRED") = ReviewRequired
parseReview Nothing = ReviewNone
parseReview _ = ReviewUnknown

checkGraphqlErrors :: Object -> Parser ()
checkGraphqlErrors row = case KM.lookup "errors" row of
    Nothing -> pure ()
    Just (Array errors) | null errors -> pure ()
    Just _ -> fail "GraphQL returned errors"

decodeValue :: Text -> (Value -> Parser a) -> Value -> IO a
decodeValue context parser value = either (remoteFailure 3 . ((context <> ": ") <>) . T.pack) pure (parseEither parser value)

decodeJson :: Text -> ByteString -> IO Value
decodeJson context bytes = either (remoteFailure 3 . ((context <> ": ") <>) . T.pack) pure (eitherDecodeStrict' bytes)

apiError :: Int -> Value -> Text
apiError status value =
    "GitHub API " <> T.pack (show status) <> ": " <> case value of
        Object row -> T.intercalate "; " (catMaybes [textField "message" row, fmap ("errors=" <>) (renderField "errors" row)])
        _ -> TE.decodeUtf8 (BL.toStrict (encode value))
  where
    textField name row = case KM.lookup name row of Just (String text) -> Just text; _ -> Nothing
    renderField name row = BL.toStrict . encode <$> KM.lookup name row >>= either (const Nothing) Just . TE.decodeUtf8'

transportFailure :: CommandResult -> IO a
transportFailure result = remoteFailure 3 ("gh transport failed" <> suffix)
  where
    suffix = case TE.decodeUtf8' (commandStderr result) of Right text | not (T.null (T.strip text)) -> ": " <> T.strip text; _ -> ""

remoteFailure :: Int -> Text -> IO a
remoteFailure code message = throwIO Failure{failureCode = code, failureDiagnostic = message}

-- | Build an escaped REST repository path for a target.
repoPath :: Target -> Text
repoPath target = "/repos/" <> encodePathSegment (targetOwner target) <> "/" <> encodePathSegment (targetRepo target)

-- | Percent-encode one path segment without allowing separators through.
encodePathSegment :: Text -> Text
encodePathSegment = T.concatMap encodeChar
  where
    encodeChar char
        | isAlphaNum char || char `elem` ("-._~" :: String) = T.singleton char
        | otherwise = T.pack (concatMap (percent . fromEnum) (T.unpack (T.singleton char)))
    percent value = '%' : pad (map toUpperHex (showHex value ""))
    pad [single] = ['0', single]
    pad digits = digits
    toUpperHex char | char >= 'a' && char <= 'f' = toEnum (ord char - 32); toUpperHex char = char

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size values = take size values : chunksOf size (drop size values)

whenEither :: Bool -> Text -> Either Text ()
whenEither condition message = if condition then Left message else Right ()
