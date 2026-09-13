{-# LANGUAGE OverloadedStrings #-}

module Myque.Github.Source (
    withSnapshot,
    decodeBlobBatch,
    resolveSourceRef,
    validatePublicationRef,
) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Myque.Github.Types
import Myque.Graph (edgesOf)
import Myque.Render (abbreviate)
import Myque.Store (
    Config (..),
    Layout (..),
    defaultConfig,
    discoverLayout,
    loadStore,
    parseConfig,
    storeSources,
 )
import Myque.Validate (findingText, validate)
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
 )
import System.Exit (ExitCode (..))
import System.FilePath (
    isAbsolute,
    joinPath,
    makeRelative,
    normalise,
    splitDirectories,
    takeDirectory,
    takeExtension,
    (</>),
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (
    byteStringInput,
    proc,
    readProcess,
    setStdin,
 )

data TreeEntry = TreeEntry
    { treeMode :: ByteString
    , treeType :: ByteString
    , treeOid :: Text
    , treeName :: FilePath
    }
    deriving (Eq, Show)

withSnapshot :: SourceSpec -> (Snapshot -> IO a) -> IO a
withSnapshot spec action = do
    root <- ensureRepositoryRoot (sourceRoot spec)
    sha <- resolveSourceRef root (sourceRef spec)
    rootEntries <- listTree root sha
    tasks <- requireEntry "missing .tasks tree" ".tasks" rootEntries
    requireTree ".tasks" tasks
    taskEntries <- listTree root (treeOid tasks)
    config <- loadConfig root taskEntries
    itemsComponents <- validateItemsPath (configItemsDir config)
    itemsEntry <- descendTree root rootEntries itemsComponents
    itemEntries <- listTree root (treeOid itemsEntry)
    let prefixed = [entry{treeName = joinPath (itemsComponents <> [treeName entry])} | entry <- itemEntries, takeExtension (treeName entry) == ".md"]
    selected <- traverse validateItemEntry (sortOn treeName prefixed)
    let oidOrder = unique (map treeOid selected <> maybe [] (pure . treeOid) (findEntry "config.toml" taskEntries))
    blobs <- if null oidOrder then pure Map.empty else readBlobs root oidOrder
    withSystemTempDirectory "myque-gh-snapshot" $ \temporaryRoot -> do
        let configPath = temporaryRoot </> ".tasks" </> "config.toml"
            itemsDir = temporaryRoot </> configItemsDir config
        createDirectoryIfMissing True itemsDir
        case findEntry "config.toml" taskEntries of
            Nothing -> pure ()
            Just entry -> do
                createDirectoryIfMissing True (takeDirectory configPath)
                BS.writeFile configPath (lookupBlob (treeOid entry) blobs)
        mapM_ (materialize temporaryRoot blobs) selected
        discovered <- discoverLayout temporaryRoot
        layout <- either (throwFailure 1 . T.pack) pure discovered
        exists <- doesDirectoryExist (layoutRoot layout </> configItemsDir (layoutConfig layout))
        unless exists (throwFailure 2 "items tree missing after materialization")
        store <- loadStore layout
        let findings = validate store
        unless (null findings) (throwFailure 1 (T.intercalate "\n" (map findingText findings)))
        let relativeSources = Map.map (normalise . makeRelative temporaryRoot) (storeSources store)
        action
            Snapshot
                { snapshotSha = sha
                , snapshotSource = spec{sourceRoot = root}
                , snapshotStore = store
                , snapshotEdges = edgesOf store
                , snapshotAbbrev = abbreviate store
                , snapshotSourcePaths = relativeSources
                }

resolveSourceRef :: FilePath -> Text -> IO Text
resolveSourceRef root ref = do
    result <- runGit root ["rev-parse", "--verify", "--end-of-options", T.unpack ref <> "^{commit}"] Nothing
    case commandExit result of
        ExitSuccess -> decodeLine 2 "invalid git commit output" (commandStdout result)
        _ -> throwFailure 2 ("cannot resolve source ref " <> ref <> diagnosticSuffix result)

validatePublicationRef :: FilePath -> Text -> IO ()
validatePublicationRef root ref = do
    unless ("refs/heads/" `T.isPrefixOf` ref && ref /= "refs/heads/") (throwFailure 2 "apply --ref must be a full refs/heads/... name")
    result <- runGit root ["check-ref-format", T.unpack ref] Nothing
    unless (commandExit result == ExitSuccess) (throwFailure 2 ("invalid publication ref: " <> ref))

ensureRepositoryRoot :: FilePath -> IO FilePath
ensureRepositoryRoot raw = do
    root <- canonicalizePath raw
    result <- runGit root ["rev-parse", "--show-toplevel"] Nothing
    actual <- case commandExit result of
        ExitSuccess -> T.unpack <$> decodeLine 2 "invalid repository root output" (commandStdout result)
        _ -> throwFailure 2 ("--store is not a Git repository root" <> diagnosticSuffix result)
    canonicalActual <- canonicalizePath actual
    unless (canonicalActual == root) (throwFailure 2 "--store must name the Git repository root")
    pure root

loadConfig :: FilePath -> [TreeEntry] -> IO Config
loadConfig root entries = case findEntry "config.toml" entries of
    Nothing -> pure defaultConfig
    Just entry -> do
        validateBlobEntry ".tasks/config.toml" entry
        bytes <- readSingleBlob root (treeOid entry)
        text <- either (const (throwFailure 2 ".tasks/config.toml is not UTF-8")) pure (TE.decodeUtf8' bytes)
        either (throwFailure 1 . T.pack) pure (parseConfig text)

validateItemsPath :: FilePath -> IO [FilePath]
validateItemsPath raw = do
    let normalized = normalise raw
        components = filter (`notElem` ["", "."]) (splitDirectories normalized)
        forbidden part = part == ".." || T.toCaseFold (T.pack part) == ".git"
    when (null raw || raw == "." || isAbsolute raw || null components || any forbidden components) $
        throwFailure 1 "configured items path must be a non-empty repository-relative path without .. or .git components"
    pure components

descendTree :: FilePath -> [TreeEntry] -> [FilePath] -> IO TreeEntry
descendTree _ _ [] = throwFailure 1 "configured items path is empty"
descendTree _ entries [name] = do
    entry <- requireEntry ("missing configured items tree: " <> T.pack name) name entries
    requireTree name entry
    pure entry
descendTree root entries (name : rest) = do
    entry <- requireEntry ("missing configured items tree component: " <> T.pack name) name entries
    requireTree name entry
    children <- listTree root (treeOid entry)
    descendTree root children rest

listTree :: FilePath -> Text -> IO [TreeEntry]
listTree root oid = do
    result <- runGit root ["ls-tree", "-z", T.unpack oid] Nothing
    case commandExit result of
        ExitSuccess -> either (throwFailure 2 . T.pack) pure (parseTree (commandStdout result))
        _ -> throwFailure 2 ("cannot read Git tree " <> oid <> diagnosticSuffix result)

parseTree :: ByteString -> Either String [TreeEntry]
parseTree raw = traverse parseOne (filter (not . BS.null) (B8.split '\0' raw))
  where
    parseOne frame = do
        let (metadata, nameWithTab) = B8.break (== '\t') frame
        nameBytes <- maybeToEither "tree entry lacks tab separator" (BS.stripPrefix "\t" nameWithTab)
        name <- either (const (Left "tree entry name is not UTF-8")) (Right . T.unpack) (TE.decodeUtf8' nameBytes)
        case B8.words metadata of
            [mode, kind, oidBytes] -> do
                oid <- either (const (Left "tree object id is not UTF-8")) Right (TE.decodeUtf8' oidBytes)
                if validOid oid
                    then Right TreeEntry{treeMode = mode, treeType = kind, treeOid = oid, treeName = name}
                    else Left "tree object id has invalid syntax"
            _ -> Left "tree entry metadata has invalid syntax"

validOid :: Text -> Bool
validOid oid = T.length oid >= 40 && T.all (`elem` ("0123456789abcdef" :: String)) oid

requireEntry :: Text -> FilePath -> [TreeEntry] -> IO TreeEntry
requireEntry message name entries = maybe (throwFailure 2 message) pure (findEntry name entries)

findEntry :: FilePath -> [TreeEntry] -> Maybe TreeEntry
findEntry name = go
  where
    go [] = Nothing
    go (entry : rest)
        | treeName entry == name = Just entry
        | otherwise = go rest

requireTree :: FilePath -> TreeEntry -> IO ()
requireTree path entry = unless (treeType entry == "tree" && treeMode entry == "040000") (throwFailure 2 (T.pack path <> " is not a Git tree"))

validateItemEntry :: TreeEntry -> IO TreeEntry
validateItemEntry entry = validateBlobEntry (T.pack (treeName entry)) entry >> pure entry

validateBlobEntry :: Text -> TreeEntry -> IO ()
validateBlobEntry path entry = unless (treeType entry == "blob" && treeMode entry `elem` ["100644", "100755"]) (throwFailure 2 (path <> " is not a regular Git blob"))

readSingleBlob :: FilePath -> Text -> IO ByteString
readSingleBlob root oid = do
    result <- runGit root ["cat-file", "blob", T.unpack oid] Nothing
    case commandExit result of
        ExitSuccess -> pure (commandStdout result)
        _ -> throwFailure 2 ("cannot read Git blob " <> oid <> diagnosticSuffix result)

readBlobs :: FilePath -> [Text] -> IO (Map Text ByteString)
readBlobs root oids = do
    let input = TE.encodeUtf8 (T.unlines oids)
    result <- runGit root ["cat-file", "--batch"] (Just input)
    case commandExit result of
        ExitSuccess -> either (throwFailure 2 . T.pack) pure (decodeBlobBatch oids (commandStdout result))
        _ -> throwFailure 2 ("cannot batch-read Git blobs" <> diagnosticSuffix result)

decodeBlobBatch :: [Text] -> ByteString -> Either String (Map Text ByteString)
decodeBlobBatch requested raw = go requested raw Map.empty
  where
    go [] remaining acc
        | BS.null remaining = Right acc
        | otherwise = Left "unexpected extra blob batch frame"
    go (expected : rest) remaining acc = do
        let (header, afterHeader0) = B8.break (== '\n') remaining
        afterHeader <- maybeToEither "truncated blob batch header" (BS.stripPrefix "\n" afterHeader0)
        (actual, size) <- parseHeader header
        if actual /= expected then Left "blob batch returned an unexpected object id" else pure ()
        let (payload, afterPayload0) = BS.splitAt size afterHeader
        whenEither (BS.length payload /= size) "truncated blob batch payload"
        afterPayload <- maybeToEither "blob batch payload lacks trailing newline" (BS.stripPrefix "\n" afterPayload0)
        go rest afterPayload (Map.insert actual payload acc)

    parseHeader header = case B8.words header of
        [oidBytes, "blob", sizeBytes] -> do
            oid <- either (const (Left "blob batch object id is not UTF-8")) Right (TE.decodeUtf8' oidBytes)
            size <- case B8.readInt sizeBytes of
                Just (n, trailing) | n >= 0 && BS.null trailing -> Right n
                _ -> Left "blob batch size is invalid"
            Right (oid, size)
        [_, kind, _] -> Left ("blob batch returned object type " <> B8.unpack kind)
        _ -> Left "blob batch header has invalid syntax"

materialize :: FilePath -> Map Text ByteString -> TreeEntry -> IO ()
materialize root blobs entry = do
    let destination = root </> joinPath (splitDirectories (treeName entry))
    createDirectoryIfMissing True (takeDirectory destination)
    BS.writeFile destination (lookupBlob (treeOid entry) blobs)

lookupBlob :: Text -> Map Text ByteString -> ByteString
lookupBlob oid blobs = Map.findWithDefault BS.empty oid blobs

unique :: (Ord a) => [a] -> [a]
unique = Map.keys . Map.fromList . map (,())

runGit :: FilePath -> [String] -> Maybe ByteString -> IO CommandResult
runGit root args stdinBytes = do
    let configured = maybe id (setStdin . byteStringInput . BL.fromStrict) stdinBytes (proc "git" ("-C" : root : args))
    (exitCode, stdoutBytes, stderrBytes) <- readProcess configured
    pure CommandResult{commandExit = exitCode, commandStdout = BL.toStrict stdoutBytes, commandStderr = BL.toStrict stderrBytes}

decodeLine :: Int -> Text -> ByteString -> IO Text
decodeLine code message bytes = do
    text <- either (const (throwFailure code message)) pure (TE.decodeUtf8' bytes)
    let stripped = T.strip text
    if T.null stripped || T.any (`elem` ['\n', '\r']) stripped then throwFailure code message else pure stripped

diagnosticSuffix :: CommandResult -> Text
diagnosticSuffix result = case TE.decodeUtf8' (commandStderr result) of
    Right text | not (T.null (T.strip text)) -> ": " <> T.strip text
    _ -> ""

throwFailure :: Int -> Text -> IO a
throwFailure code message = throwIO Failure{failureCode = code, failureDiagnostic = message}

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither message = maybe (Left message) Right

whenEither :: Bool -> e -> Either e ()
whenEither condition message = if condition then Left message else Right ()
