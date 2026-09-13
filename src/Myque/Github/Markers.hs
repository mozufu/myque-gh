{-# LANGUAGE OverloadedStrings #-}

-- | Trusted machine markers embedded in issue and pull-request bodies.
module Myque.Github.Markers (
    MarkerError (..),
    ParsedPrLinks (..),
    parseIssueIdentity,
    renderIssueIdentity,
    parsePrLinks,
    rewritePrLinks,
) where

import Data.Char (isSpace)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Uuid (Uuid, isUuidV7, parseUuid, uuidText)

-- | Invalid or ambiguous managed marker syntax.
data MarkerError = MarkerError Text
    deriving (Eq, Show)

-- | Parsed work-item links and any trailer diagnostics.
data ParsedPrLinks = ParsedPrLinks
    { parsedPrUuids :: Set Uuid
    , parsedPrDiagnostics :: [Text]
    , parsedPrSpan :: Maybe (Int, Int)
    }
    deriving (Eq, Show)

issuePrefix :: Text
issuePrefix = "<!-- myque:id="

projectionLine :: Text
projectionLine = "<!-- myque:projection=github/v1 -->"

-- | Parse a managed issue identity only when it begins the body.
parseIssueIdentity :: Text -> Either MarkerError (Maybe Uuid)
parseIssueIdentity body = case normalizedLines body of
    first : second : rest
        | issuePrefix `T.isPrefixOf` first -> do
            uuid <- parseIssueLine first
            if second /= projectionLine
                then Left (MarkerError "identity header lacks supported github/v1 projection line")
                else
                    if any (issuePrefix `T.isPrefixOf`) rest
                        then Left (MarkerError "identity header is duplicated")
                        else Right (Just uuid)
    first : _
        | issuePrefix `T.isPrefixOf` first -> Left (MarkerError "identity header is incomplete")
    _ -> Right Nothing

-- | Render the trusted two-line managed issue identity header.
renderIssueIdentity :: Uuid -> Text
renderIssueIdentity uuid = issuePrefix <> uuidText uuid <> " -->\n" <> projectionLine <> "\n"

parseIssueLine :: Text -> Either MarkerError Uuid
parseIssueLine line = do
    raw <- maybe (Left (MarkerError "malformed myque issue identity")) Right (T.stripSuffix " -->" =<< T.stripPrefix issuePrefix line)
    uuid <- either (Left . MarkerError . T.pack) Right (parseUuid raw)
    if isUuidV7 uuid then Right uuid else Left (MarkerError "myque issue identity is not UUIDv7")

startMarker :: Text
startMarker = "<!-- myque:pr-links=github/v1 -->"

endMarker :: Text
endMarker = "<!-- myque:pr-links:end -->"

implementsPrefix :: Text
implementsPrefix = "<!-- myque:implements="

-- | Parse the optional managed trailer at the end of a pull-request body.
parsePrLinks :: Text -> ParsedPrLinks
parsePrLinks body = case parseTrailer body of
    Right Nothing -> ParsedPrLinks Set.empty [] Nothing
    Right (Just (start, end, uuids)) -> ParsedPrLinks uuids [] (Just (start, end))
    Left message -> ParsedPrLinks Set.empty [message] Nothing

-- | Replace or remove the managed pull-request trailer without touching human text.
rewritePrLinks :: Text -> Set Uuid -> Either MarkerError Text
rewritePrLinks body desired = case parseTrailer body of
    Left message -> Left (MarkerError message)
    Right Nothing
        | Set.null desired -> Right body
        | otherwise -> Right (body <> separator body <> renderTrailer desired <> "\n")
    Right (Just (start, end, existing))
        | existing == desired -> Right body
        | otherwise ->
            let (before, rest) = splitAtLine start body
                (_, after) = splitAtLine (end - start + 1) rest
             in if Set.null desired
                    then Right (trimTrailerSeparator before <> after)
                    else Right (before <> renderTrailer desired <> lineEndingAt body start <> after)

parseTrailer :: Text -> Either Text (Maybe (Int, Int, Set Uuid))
parseTrailer body = do
    let linesWithEnds = splitLines body
        contents = map fst linesWithEnds
        nonEmpty = [index | (index, line) <- zip [0 ..] contents, not (T.null (T.strip line))]
        markerLine line = any (`T.isInfixOf` line) ["myque:pr-links", "myque:implements"]
    case reverse nonEmpty of
        [] -> Right Nothing
        lastIndex : _
            | contents !! lastIndex == endMarker -> do
                start <- maybe (Left "myque PR trailer lacks start marker") Right (lastMatchingBefore startMarker lastIndex contents)
                whenText (insideFence contents start) "myque PR trailer starts inside an unclosed Markdown fence"
                whenText (any (== startMarker) (take start contents)) "multiple top-level myque PR trailers"
                uuids <- traverse parseImplements [T.strip line | line <- take (lastIndex - start - 1) (drop (start + 1) contents), not (T.null (T.strip line))]
                Right (Just (start, lastIndex, Set.fromList uuids))
            | markerLine (contents !! lastIndex) || any markerLine (drop (lastIndex + 1) contents) -> Left "malformed myque PR trailer candidate at end of body"
            | otherwise -> Right Nothing

parseImplements :: Text -> Either Text Uuid
parseImplements line
    | line == startMarker = Left "nested myque PR trailer start"
    | line == endMarker = Left "nested myque PR trailer end"
    | otherwise = do
        raw <- maybe (Left "unknown content inside myque PR trailer") Right (T.stripSuffix " -->" =<< T.stripPrefix implementsPrefix line)
        uuid <- either (Left . T.pack) Right (parseUuid raw)
        if isUuidV7 uuid then Right uuid else Left "myque PR link is not UUIDv7"

renderTrailer :: Set Uuid -> Text
renderTrailer uuids = T.unlines ([startMarker] <> map (\uuid -> implementsPrefix <> uuidText uuid <> " -->") (Set.toAscList uuids) <> [endMarker])

normalizedLines :: Text -> [Text]
normalizedLines = map (T.dropWhileEnd (== '\r')) . T.splitOn "\n"

splitLines :: Text -> [(Text, Text)]
splitLines body = go body
  where
    go text = case T.breakOn "\n" text of
        (line, "") -> [(T.dropWhileEnd (== '\r') line, "")]
        (line, rest) ->
            let ending = if T.isSuffixOf "\r" line then "\r\n" else "\n"
             in (T.dropWhileEnd (== '\r') line, ending) : go (T.drop 1 rest)

lastMatchingBefore :: Text -> Int -> [Text] -> Maybe Int
lastMatchingBefore needle bound lines' = case [i | (i, line) <- zip [0 .. bound - 1] lines', line == needle] of
    [] -> Nothing
    matches -> Just (last matches)

data Fence = Fence
    { fenceChar :: Char
    , fenceLength :: Int
    , fenceTrailing :: Text
    }

insideFence :: [Text] -> Int -> Bool
insideFence lines' stop = maybe False (const True) (foldl track Nothing (take stop lines'))
  where
    track Nothing line = fenceToken line
    track current@(Just opening) line = case fenceToken line of
        Just candidate
            | closes opening candidate -> Nothing
        _ -> current
    closes opening candidate =
        fenceChar candidate == fenceChar opening
            && fenceLength candidate >= fenceLength opening
            && T.all isSpace (fenceTrailing candidate)

fenceToken :: Text -> Maybe Fence
fenceToken line =
    let stripped = T.dropWhile (== ' ') line
        indent = T.length line - T.length stripped
     in if indent > 3
            then Nothing
            else case T.uncons stripped of
                Just (char, _)
                    | char == '`' || char == '~' ->
                        let run = T.takeWhile (== char) stripped
                            count = T.length run
                         in if count >= 3
                                then Just Fence{fenceChar = char, fenceLength = count, fenceTrailing = T.drop count stripped}
                                else Nothing
                _ -> Nothing

splitAtLine :: Int -> Text -> (Text, Text)
splitAtLine count body =
    let chunks = splitLines body
        renderChunk (line, ending) = line <> ending
     in (T.concat (map renderChunk (take count chunks)), T.concat (map renderChunk (drop count chunks)))

lineEndingAt :: Text -> Int -> Text
lineEndingAt body index = case drop index (splitLines body) of
    (_, ending) : _ | not (T.null ending) -> ending
    _ -> "\n"

trimTrailerSeparator :: Text -> Text
trimTrailerSeparator text
    | T.isSuffixOf "\r\n\r\n" text = T.dropEnd 4 text
    | T.isSuffixOf "\n\n" text = T.dropEnd 2 text
    | otherwise = text

separator :: Text -> Text
separator body
    | T.null body = ""
    | T.isSuffixOf "\n\n" body || T.isSuffixOf "\r\n\r\n" body = ""
    | T.isSuffixOf "\n" body = "\n"
    | otherwise = "\n\n"

whenText :: Bool -> Text -> Either Text ()
whenText condition message = if condition then Left message else Right ()
