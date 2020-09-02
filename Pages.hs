{-# LANGUAGE TypeApplications #-}
module Pages (
    Pages(..), pagesFromDisk
) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Enc
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import System.Exit (die)
import qualified Text.Mustache as Mustache
import Text.Mustache (toMustache)

import DB (KeyType, ContentsType)


data Pages = Pages { pIndex :: ByteString
                   , pPasteRead :: KeyType -> Maybe POSIXTime -> ContentsType -> ByteString }

pagesFromDisk :: IO Pages
pagesFromDisk = Pages <$> BS.readFile "index.html"
                      <*> (renderReadPage <$> loadTemplate "read.mustache")

loadTemplate :: FilePath -> IO Mustache.Template
loadTemplate fp = do
    res <- Mustache.localAutomaticCompile fp
    case res of
        Right templ -> return templ
        Left err -> die (show err)

renderReadPage :: Mustache.Template -> KeyType -> Maybe POSIXTime -> ContentsType -> ByteString
renderReadPage templ key mdate files = Enc.encodeUtf8 $
    Mustache.substituteValue templ $ Mustache.object
        [(Text.pack "key", toMustache (Enc.decodeUtf8 key))
        ,(Text.pack "date", maybe (toMustache False) (toMustache . show . posixSecondsToUTCTime) mdate)
        ,(Text.pack "unixdate", maybe (toMustache False) (toMustache @Int . truncate . nominalDiffTimeToSeconds) mdate)
        ,(Text.pack "pastes", toMustache
            [Mustache.object
                [(Text.pack "fname", maybe (toMustache False) (toMustache . Enc.decodeUtf8) mfname)
                ,(Text.pack "contents", toMustache (Enc.decodeUtf8 contents))]
            | (mfname, contents) <- files])]
