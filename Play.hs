{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Play (playModule) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import Snap.Core hiding (path, method)
import System.Exit (ExitCode(..))
import qualified System.IO.Streams as Streams
import System.IO.Streams (InputStream)
import qualified Text.JSON as JSON
import Text.JSON (JSValue(..))
import Text.JSON.String (runGetJSON)
import Text.Read (readMaybe)

import GHCPool
import Paste.DB (getPaste, Contents(..))
import ServerModule
import SpamDetect hiding (Action(..))
import qualified SpamDetect as Spam (Action(..))


data Context = Context Pool

data WhatRequest
  = Index
  | FromPaste ByteString (Maybe Int)
  | Versions
  | Run
  | Core
  | Asm
  deriving (Show)

parseRequest :: Method -> [ByteString] -> Maybe WhatRequest
parseRequest method comps = case (method, comps) of
  (GET, ["play"]) -> Just Index
  (GET, ["play", "paste", key]) -> Just (FromPaste key Nothing)
  (GET, ["play", "paste", key, idxs])
    | Just idx <- readMaybe (Char8.unpack idxs) -> Just (FromPaste key (Just idx))
  (GET, ["play", "versions"]) -> Just Versions
  (POST, ["play", "run"]) -> Just Run
  (POST, ["play", "core"]) -> Just Core
  (POST, ["play", "asm"]) -> Just Asm
  _ -> Nothing

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

handleRequest :: GlobalContext -> Context -> WhatRequest -> Snap ()
handleRequest gctx (Context pool) = \what -> case what of
  Index -> staticFile "text/html" "play.html"

  FromPaste key midx -> do
    res <- liftIO $ getPaste (gcDb gctx) key
    case (res, midx) of
      -- TODO: Actually put this in a playground page instead of returning the text as-is
      (Just (_, Contents ((_, source):_) _ _), Nothing) ->
        writeBS source
      (Just (_, Contents l _ _), Just idx)
        | idx >= 1
        , (_, source) : _ <- drop (idx - 1) l ->
            writeBS source

      (Just (_, Contents [] _ _), Nothing) -> do
        modifyResponse (setContentType (Char8.pack "text/plain"))
        writeBS (Char8.pack "That paste seems to have no files?")
      (Just (_, Contents _ _ _), Just _) -> do
        modifyResponse (setContentType (Char8.pack "text/plain"))
        writeBS (Char8.pack "File index out of range")
      (Nothing, _) -> do
        modifyResponse (setContentType (Char8.pack "text/plain"))
        writeBS (Char8.pack "That paste does not exist!")

  Versions -> do
    modifyResponse (setContentType (Char8.pack "text/plain"))
    versions <- liftIO availableVersions
    writeJSON $ JSArray (map (JSString . JSON.toJSString) versions)

  _ -> do
    req <- getRequest
    isSpam <- liftIO $ recordCheckSpam Spam.PlayRunStart (gcSpam gctx) (rqClientAddr req)
    if isSpam
      then httpError 429 "Please slow down a bit, you're rate limited"
      else do mpostdata <- runRequestBody $ \stream ->
                streamReadMaxN 100000 stream >>= \case
                  Nothing -> return $ Left (413, "Program too large")
                  Just s -> return (Right s)
              case mpostdata of
                Left (code, err) -> httpError code err
                Right postdata -> do
                  case runGetJSON JSON.readJSValue (UTF8.toString postdata) of
                    Right (JSObject (JSON.fromJSObject -> obj))
                      | Just (JSString (JSON.fromJSString -> source))  <- lookup "source" obj
                      , Just (JSString (JSON.fromJSString -> version)) <- lookup "version" obj
                      , Just (JSString (JSON.fromJSString -> opt))     <- lookup "opt" obj
                      -> do runner <- case what of
                                        Run -> pure CRun
                                        Core -> pure CCore
                                        Asm -> pure CAsm
                                        _ -> fail ("Unexpexted what value: " <> show what)
                            res <- liftIO $ runInPool pool runner (Version version) (read opt) source
                            case res of
                              Left err -> httpError 500 err
                              Right result -> do
                                -- Record the run as a spam-checking action, but don't actually act
                                -- on the return value yet; that will come on the next user action
                                let timeFraction = realToFrac (resTimeTaken result) / (fromIntegral runTimeoutMicrosecs / 1e6)
                                _ <- liftIO $ recordCheckSpam (Spam.PlayRunTimeoutFraction timeFraction) (gcSpam gctx) (rqClientAddr req)

                                modifyResponse (setContentType (Char8.pack "text/json"))
                                writeJSON $ JSON.makeObj
                                              [("ec", JSRational False (fromIntegral (exitCode (resExitCode result))))
                                              ,("out", JSString (JSON.toJSString (resStdout result)))
                                              ,("err", JSString (JSON.toJSString (resStderr result)))]
                    _ -> httpError 400 "Invalid JSON"

playModule :: ServerModule
playModule = ServerModule
  { smMakeContext = \_gctx _options k -> do
      nprocs <- getNumCapabilities
      -- TODO: the max queue length is a completely arbitrary value
      pool <- makePool nprocs nprocs
      k (Context pool)
  , smParseRequest = parseRequest
  , smHandleRequest = handleRequest
  , smStaticFiles = [("bundle.js", "text/javascript")]
  , smReloadPages = \_ -> return ()
  }

writeJSON :: JSValue -> Snap ()
writeJSON = writeLBS . BSB.toLazyByteString . BSB.stringUtf8 . flip JSON.showJSValue ""

exitCode :: ExitCode -> Int
exitCode ExitSuccess = 0
exitCode (ExitFailure n) = n
