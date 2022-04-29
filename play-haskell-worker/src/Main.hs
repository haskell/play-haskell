{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import Data.ByteString (ByteString)
import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Snap.Core hiding (path, method)
import Snap.Http.Server
import System.IO
import qualified Text.JSON as JSON

import PlayHaskellTypes (RunRequest(..), RunResponse(..))
import Snap.Server.Utils.ExitEarly
import Snap.Server.Utils
import qualified Snap.Server.Utils.Options as Opt
import Snap.Server.Utils.Shim

import GHCPool


data WhatRequest
  = SubmitJob
  deriving (Show)

parseRequest :: Method -> [ByteString] -> Maybe WhatRequest
parseRequest method comps = case (method, comps) of
  (POST, ["job"]) -> Just SubmitJob
  _ -> Nothing

handleRequest :: Pool -> WhatRequest -> Snap ()
handleRequest pool = \case
  SubmitJob -> execExitEarlyT $ do
    -- TODO: check signing keys here

    mpostdata <- lift $ runRequestBody $ \stream ->
      streamReadMaxN 1000_000 stream >>= \case
        Nothing -> return $ Left (413, "Program too large")
        Just s -> return (Right s)
    postdata <- okOrExitEarly mpostdata $ \(code, err) -> lift (httpError code err)

    runreq <- case JSON.decode (UTF8.toString postdata) of
      JSON.Ok runreq -> return runreq
      _ -> do lift (httpError 400 "Invalid JSON")
              exitEarly ()

    result <- liftIO $ runInPool pool (runreqCommand runreq)
                                      (runreqVersion runreq)
                                      (runreqOpt runreq)
                                      (runreqSource runreq)
    let response = case result of
          Left err -> RunResponseErr err
          Right res -> RunResponseOk
                         { runresExitCode = resExitCode res
                         , runresStdout = resStdout res
                         , runresStderr = resStderr res
                         , runresTimeTakenSecs = resTimeTaken res }
    
    lift $ modifyResponse (setContentType (Char8.pack "text/json"))
    lift $ writeLBS . BSB.toLazyByteString . BSB.stringUtf8 $ JSON.encode response

splitPath :: ByteString -> Maybe [ByteString]
splitPath path
  | BS.null path || BS.head path /= fromIntegral (ord '/')
  = Nothing
splitPath path = Just (BS.split (fromIntegral (ord '/')) (trimSlashes path))
  where
    trimSlashes :: ByteString -> ByteString
    trimSlashes = let slash = fromIntegral (ord '/')
                  in BS.dropWhile (== slash) . BS.dropWhileEnd (== slash)

server :: Options -> Pool -> Snap ()
server options pool = do
  -- If we're proxied, set the source IP from the X-Forwarded-For header.
  when (oProxied options) ipHeaderFilterSupportingIPv6

  req <- getRequest
  let path = rqContextPath req `BS.append` rqPathInfo req
      method = rqMethod req

  case splitPath path of
    Just components
      | Just what <- parseRequest method components ->
          handleRequest pool what
      | otherwise ->
          httpError 404 "Path not found"
    Nothing -> httpError 400 "Invalid URL"

config :: Config Snap a
config =
  let stderrlogger = ConfigIoLog (Char8.hPutStrLn stderr)
  in setAccessLog stderrlogger
     . setErrorLog stderrlogger
     . setPort 8124
     $ defaultConfig

data Options = Options { oProxied :: Bool }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options False

main :: IO ()
main = do
  options <- Opt.parseOptions $ Opt.Interface defaultOptions $ Map.fromList
    [("--proxied", Opt.Flag "Assumes the server is running behind a proxy that sets \
                            \X-Forwarded-For, instead of using the source IP of a \
                            \request for rate limiting."
                            (\o -> o { oProxied = True }))
    ,("--help", Opt.Help)
    ,("-h", Opt.Help)]

  nprocs <- getNumCapabilities
  pool <- makePool nprocs

  httpServe config (server options pool)
