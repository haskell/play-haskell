{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Play (playModule) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Snap.Core hiding (path, method)
import qualified System.IO.Streams as Streams
import System.IO.Streams (InputStream)

import GHCPool
import ServerModule
import SpamDetect hiding (Action(..))
import qualified SpamDetect as Spam (Action(..))


data Context = Context Pool

data WhatRequest
  = Index
  | Run
  deriving (Show)

parseRequest :: Method -> [ByteString] -> Maybe WhatRequest
parseRequest method comps = case (method, comps) of
  (GET, ["play"]) -> Just Index
  (POST, ["play", "run"]) -> Just Run
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
handleRequest gctx (Context pool) = \case
  Index -> staticFile "text/html" "play.html"
  Run -> do
    req <- getRequest
    isSpam <- liftIO $ recordCheckSpam Spam.PlayRun (gcSpam gctx) (rqClientAddr req)
    if isSpam
        then httpError 429 "Please slow down a bit, you're rate limited"
        else do res <- runRequestBody $ \stream ->
                  streamReadMaxN 100000 stream >>= \case
                    Nothing -> return $ Left (413, "Program too large")
                    Just source -> do
                      res <- runInPool pool CRun (Version "8.10.7") (Char8.unpack source)
                      case res of
                        Left err -> return $ Left (500, err)
                        Right ok -> return $ Right ok
                case res of
                  Left (code, err) -> httpError code err
                  Right (ec, out, err) -> writeBS (JSON.encode [ec, out, err])

playModule :: ServerModule
playModule = ServerModule
  { smMakeContext = \_options k -> do
      nprocs <- getNumCapabilities
      -- TODO: the max queue length is a completely arbitrary value
      pool <- makePool nprocs nprocs
      k (Context pool)
  , smParseRequest = parseRequest
  , smHandleRequest = handleRequest
  , smStaticFiles = [("play-index.js", "text/javascript")]
  , smReloadPages = \_ -> return ()
  }
