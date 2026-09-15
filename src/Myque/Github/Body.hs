{-# LANGUAGE OverloadedStrings #-}

-- | Consumer-configured Markdown rendering. No consumer schema is interpreted here.
module Myque.Github.Body (
    BodyRenderer (..),
    prepareBodies,
    validateProjectionBodies,
    projectionBody,
) where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (unless, when)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Myque.Api (itemApiValue)
import Myque.Github.Types
import Myque.Item (WorkItem (..), bodyAfterTitle)
import Myque.Store (Store (..))
import Myque.Uuid (uuidText)
import System.Exit (ExitCode (..))
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)

-- | An executable and literal arguments supplied by the operator, never the item.
data BodyRenderer = BodyRenderer FilePath [String]
    deriving (Eq, Show)

-- | Render all active records before remote discovery; a failure never falls back.
prepareBodies :: Maybe BodyRenderer -> Snapshot -> IO Snapshot
prepareBodies renderer snapshot = do
    rendered <- case renderer of
        Nothing -> pure Map.empty
        Just (BodyRenderer executable arguments) -> do
            when (null executable || any (elem '\0') (executable : arguments)) (refuse "invalid body renderer argument vector")
            Map.traverseWithKey (render executable arguments) active
    let prepared = snapshot{snapshotRenderedBodies = rendered}
    either refuse pure (validateProjectionBodies prepared)
    pure prepared
  where
    store = snapshotStore snapshot
    active = Map.difference (storeById store) (storeTerminals store)
    render executable arguments uuid item = do
        (code, output, diagnostic) <-
            readProcess (setStdin (byteStringInput (encode (itemApiValue store item))) (proc executable arguments))
                `catch` \exception -> refuse ("body renderer could not start: " <> T.pack (show (exception :: IOException)))
        unless (code == ExitSuccess) (refuse ("body renderer failed for " <> uuidText uuid <> ": " <> TE.decodeUtf8With (\_ _ -> Just '\xfffd') (BL.toStrict diagnostic)))
        markdown <- either (const (refuse "body renderer returned invalid UTF-8")) pure (TE.decodeUtf8' (BL.toStrict output))
        when (T.null (T.strip markdown) && not (T.null (T.strip (itemBody item)))) (refuse ("body renderer returned empty Markdown for " <> uuidText uuid))
        when ("<!-- myque:" `T.isInfixOf` markdown) (refuse ("body renderer returned reserved identity markers for " <> uuidText uuid))
        pure markdown

{- | A presentation safeguard, not a Zutai parser or profile validator.
Structured code fences must never become the human requirements description.
-}
validateProjectionBodies :: Snapshot -> Either Text ()
validateProjectionBodies snapshot = mapM_ validateBody (Map.elems active)
  where
    store = snapshotStore snapshot
    active = Map.difference (storeById store) (storeTerminals store)
    -- Guard exactly what a reader sees: the emitted text, and (without a
    -- renderer, where the emitted text still carries the title line) the
    -- description that follows the title.
    validateBody item
        | any startsStructured (projected item) =
            Left ("structured requirements require a configured readable Markdown renderer: " <> uuidText (itemId item))
        | otherwise = Right ()
    projected item = case Map.lookup (itemId item) (snapshotRenderedBodies snapshot) of
        Just markdown -> [markdown]
        Nothing -> [itemBody item, bodyAfterTitle item]
    startsStructured body = case dropWhile (T.null . T.strip) (T.lines body) of
        first : _ -> structuredFence first
        [] -> False
    structuredFence line =
        let stripped = T.stripStart line
            fence = T.takeWhile (== '`') stripped
            tildeFence = T.takeWhile (== '~') stripped
            language prefix = T.toCaseFold (T.takeWhile (not . isSpace) (T.stripStart (T.drop (T.length prefix) stripped)))
         in (T.length fence >= 3 && language fence `elem` ["zt", "zti"])
                || (T.length tildeFence >= 3 && language tildeFence `elem` ["zt", "zti"])

projectionBody :: Snapshot -> WorkItem -> Text
projectionBody snapshot item
    | Map.member (itemId item) (storeTerminals (snapshotStore snapshot)) =
        "This item is retired. Its full historical description is not available offline. Use the immutable canonical history reference above to retrieve and verify it through MyQue.\n"
    | otherwise = Map.findWithDefault (itemBody item) (itemId item) (snapshotRenderedBodies snapshot)

refuse :: Text -> IO a
refuse message = throwIO Failure{failureCode = 1, failureDiagnostic = message}
