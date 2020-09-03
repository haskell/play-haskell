{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Pages (
    Pages(..), pagesFromDisk
) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import System.Exit (die)
import qualified Text.Mustache as Mustache
import Text.Mustache (toMustache)
import qualified Text.Mustache.Types as Mustache (Value)

import DB (KeyType, ContentsType)


data Pages = Pages { pIndex :: ContentsType -> ByteString  -- pass empty content for new blank paste
                   , pPasteRead :: KeyType -> Maybe POSIXTime -> ContentsType -> ByteString }

pagesFromDisk :: IO Pages
pagesFromDisk = Pages <$> (renderIndexPage <$> loadTemplate "index.mustache")
                      <*> (renderReadPage <$> loadTemplate "read.mustache")

loadTemplate :: FilePath -> IO Mustache.Template
loadTemplate fp = do
    res <- Mustache.localAutomaticCompile fp
    case res of
        Right templ -> return templ
        Left err -> die (show err)

renderIndexPage :: Mustache.Template -> ContentsType -> ByteString
renderIndexPage templ [] = renderIndexPage templ [(Nothing, BS.empty)]  -- single empty file if no files given
renderIndexPage templ files = Enc.encodeUtf8 $
    Mustache.substituteValue templ $ Mustache.object
        [(Text.pack "pastes", mixinPasteList files)]

renderReadPage :: Mustache.Template -> KeyType -> Maybe POSIXTime -> ContentsType -> ByteString
renderReadPage templ key mdate files = Enc.encodeUtf8 $
    Mustache.substituteValue templ $ Mustache.object
        [(Text.pack "key", toMustache (Enc.decodeUtf8 key))
        ,(Text.pack "date", mixinMaybeNull (show . posixSecondsToUTCTime) mdate)
        ,(Text.pack "unixdate", mixinMaybeNull @Int (truncate . nominalDiffTimeToSeconds) mdate)
        ,(Text.pack "pastes", mixinPasteList files)]

mixinPasteList :: ContentsType -> Mustache.Value
mixinPasteList = toMustache . zipWith mixinSinglePaste [1..]

mixinSinglePaste :: Int -> (Maybe ByteString, ByteString) -> Mustache.Value
mixinSinglePaste index (mfname, contents) = Mustache.object $
    [(Text.pack "fname", mixinMaybeNull Enc.decodeUtf8 mfname)
    ,(Text.pack "fnameAttr", maybe (toMustache "") (toMustache . escapeAttribute . Enc.decodeUtf8) mfname)
    ,(Text.pack "contents", toMustache (Enc.decodeUtf8 contents))
    ,(Text.pack "index", toMustache index)
    ,(Text.pack "firstFile", toMustache (index == 1))]

mixinMaybeNull :: Mustache.ToMustache b => (a -> b) -> Maybe a -> Mustache.Value
mixinMaybeNull _ Nothing = toMustache False
mixinMaybeNull f (Just x) = toMustache (f x)

escapeAttribute :: Text -> Text
escapeAttribute text
  | Text.any (`elem` "\x00<>&\"") text =
      flip Text.concatMap text $ \case
          '\x00' -> Text.empty
          '<' -> Text.pack "&lt;"
          '>' -> Text.pack "&gt;"
          '&' -> Text.pack "&amp;"
          '"' -> Text.pack "&quot;"
          c -> Text.singleton c
  | otherwise = text
