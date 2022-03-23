{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Paste.Pages (
    Pages(..), pagesFromDisk
) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Encoding.Error as Enc (lenientDecode)
import Data.Time.Clock (nominalDiffTimeToSeconds, nominalDay)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import System.Exit (die)
import qualified Text.Mustache as Mustache
import Text.Mustache (toMustache)
import qualified Text.Mustache.Types as Mustache (Value)

import Paste.DB (KeyType, Contents(..))


data Pages = Pages { pIndex :: Contents -> ByteString  -- pass empty content for new blank paste
                   , pPasteRead :: POSIXTime -> KeyType -> Maybe POSIXTime -> Contents -> ByteString }

pagesFromDisk :: IO Pages
pagesFromDisk = Pages <$> (renderIndexPage <$> loadTemplate "index.mustache")
                      <*> (renderReadPage <$> loadTemplate "read.mustache")

loadTemplate :: FilePath -> IO Mustache.Template
loadTemplate fp = do
    res <- Mustache.localAutomaticCompile fp
    case res of
        Right templ -> return templ
        Left err -> die (show err)

renderIndexPage :: Mustache.Template -> Contents -> ByteString
renderIndexPage templ (Contents [] mparent mexpire) =
    -- single empty file if no files given
    renderIndexPage templ (Contents [(Nothing, BS.empty)] mparent mexpire)
renderIndexPage templ (Contents files mparent _) = Enc.encodeUtf8 $
    Mustache.substituteValue templ $ Mustache.object
        [(Text.pack "pastes", mixinPasteList files)
        ,(Text.pack "parentAttr", mixinMaybeNull (escapeAttribute . decodeUtf8) mparent)]

renderReadPage :: Mustache.Template -> POSIXTime -> KeyType -> Maybe POSIXTime -> Contents -> ByteString
renderReadPage templ now key mdate (Contents files mparent mexpire) = Enc.encodeUtf8 $
    Mustache.substituteValue templ $ Mustache.object
        [(Text.pack "key", toMustache (decodeUtf8 key))
        ,(Text.pack "date", mixinMaybeNull (show . posixSecondsToUTCTime) mdate)
        ,(Text.pack "unixdate", mixinMaybeNull @Int (truncate . nominalDiffTimeToSeconds) mdate)
        ,(Text.pack "pastes", mixinPasteList files)
        ,(Text.pack "parent", mixinMaybeNull decodeUtf8 mparent)
        ,(Text.pack "parentAttr", mixinMaybeNull (escapeAttribute . decodeUtf8) mparent)
        ,(Text.pack "expirenote", mixinMaybeNull (renderExpire now) mexpire)]

mixinPasteList :: [(Maybe ByteString, ByteString)] -> Mustache.Value
mixinPasteList = toMustache . zipWith mixinSinglePaste [1..]

mixinSinglePaste :: Int -> (Maybe ByteString, ByteString) -> Mustache.Value
mixinSinglePaste index (mfname, contents) = Mustache.object
    [(Text.pack "fname", mixinMaybeNull decodeUtf8 mfname)
    ,(Text.pack "fnameAttr", maybe (toMustache "") (toMustache . escapeAttribute . decodeUtf8) mfname)
    ,(Text.pack "contents", toMustache (decodeUtf8 contents))
    ,(Text.pack "linenums", toMustache (unlines (map show [1 .. numlines])))
    ,(Text.pack "index", toMustache index)
    ,(Text.pack "firstFile", toMustache (index == 1))]
  where
    numlines = BS.count 10 contents + (case BS.unsnoc contents of Just (_, 10) -> 0 ; _ -> 1)

mixinMaybeNull :: Mustache.ToMustache b => (a -> b) -> Maybe a -> Mustache.Value
mixinMaybeNull _ Nothing = toMustache False
mixinMaybeNull f (Just x) = toMustache (f x)

renderExpire :: POSIXTime -> POSIXTime -> Text
renderExpire now expire =
    case expire - now of
      left | left <= 1.01 * nominalDay -> Text.pack "This paste expires soon."
           | left <= 7.01 * nominalDay -> Text.pack "This paste expires in a few days."
           | left <= 31.01 * nominalDay -> Text.pack "This paste expires in a few weeks."
           | otherwise -> Text.pack "This paste will expire at some point."

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

decodeUtf8 :: ByteString -> Text
decodeUtf8 = Enc.decodeUtf8With Enc.lenientDecode
