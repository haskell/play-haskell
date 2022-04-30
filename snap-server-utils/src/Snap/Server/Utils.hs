{-# LANGUAGE LambdaCase #-}
module Snap.Server.Utils where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import Data.String (fromString)
import Snap.Core hiding (path)
import System.FilePath ((</>))
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import Text.JSON (JSON)
import qualified Text.JSON as JSON

import Snap.Server.Utils.ExitEarly


httpError :: Int -> String -> Snap ()
httpError code msg = do
  putResponse $ setResponseCode code emptyResponse
  writeBS (Char8.pack msg)

applyStaticFileHeaders :: String -> Response -> Response
applyStaticFileHeaders mime =
  setContentType (Char8.pack mime)
  . setHeader (fromString "Cache-Control") (Char8.pack "public max-age=3600")

-- | Suggestion: overload this to `staticFile` in your local project by
-- partially applying this to the path of your static files directory.
staticFile' :: FilePath -> String -> FilePath -> Snap ()
staticFile' prefixPath mime path = do
  modifyResponse (applyStaticFileHeaders mime)
  sendFile (prefixPath </> path)

writeHTML :: MonadSnap m => ByteString -> m ()
writeHTML bs = do
  modifyResponse $ setContentType (Char8.pack "text/html; charset=utf-8")
  writeBS bs

streamReadMaxN :: Int -> InputStream ByteString -> IO (Maybe ByteString)
streamReadMaxN maxlen stream = fmap mconcat <$> go 0
  where go :: Int -> IO (Maybe [ByteString])
        go yet = Streams.read stream >>= \case
                   Nothing -> return (Just [])
                   Just chunk
                     | yet + BS.length chunk <= maxlen ->
                         fmap (chunk :) <$> go (yet + BS.length chunk)
                     | otherwise ->
                         return Nothing

-- | `getRequestBodyEarlyExit sizelimit oversizeerror`.
--
-- If the request body is greater than `sizelimit`, early-exits with HTTP code
-- 413 Payload Too Large with the body text given by `oversizeerror`.
getRequestBodyEarlyExit :: Int -> String -> ExitEarlyT () Snap ByteString
getRequestBodyEarlyExit sizelimit oversizeerror = do
  mbody <- lift $ runRequestBody $ \stream ->
    streamReadMaxN sizelimit stream >>= \case
      Nothing -> return $ Left (413, oversizeerror)
      Just s -> return (Right s)
  body <- okOrExitEarly mbody $ \(code, err) -> lift (httpError code err)
  return body

-- | Same as `getRequestBodyEarlyExit`, except the body is also JSON-decoded.
getRequestBodyEarlyExitJSON :: JSON a => Int -> String -> ExitEarlyT () Snap a
getRequestBodyEarlyExitJSON sizelimit oversizeerror = do
  postdata <- getRequestBodyEarlyExit sizelimit oversizeerror
  case JSON.decode (UTF8.toString postdata) of
    JSON.Ok value -> return value
    _ -> do lift (httpError 400 "Invalid JSON")
            exitEarly ()
