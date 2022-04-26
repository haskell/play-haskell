{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Snap.Core hiding (path, method)
import Snap.Http.Server
import System.IO

import Snap.Server.Utils
import qualified Snap.Server.Utils.Options as Opt
import Snap.Server.Utils.Shim

import GHCPool


data WhatRequest
  = RunGHC Command
  deriving (Show)

parseRequest :: Method -> [ByteString] -> Maybe WhatRequest
parseRequest method comps = case (method, comps) of
  (POST, ["job", "run"]) -> Just (RunGHC CRun)
  (POST, ["job", "core"]) -> Just (RunGHC CCore)
  (POST, ["job", "asm"]) -> Just (RunGHC CAsm)
  _ -> Nothing

handleRequest :: WhatRequest -> Snap ()
handleRequest = \case
  _ -> _

splitPath :: ByteString -> Maybe [ByteString]
splitPath path
  | BS.null path || BS.head path /= fromIntegral (ord '/')
  = Nothing
splitPath path = Just (BS.split (fromIntegral (ord '/')) (trimSlashes path))
  where
    trimSlashes :: ByteString -> ByteString
    trimSlashes = let slash = fromIntegral (ord '/')
                  in BS.dropWhile (== slash) . BS.dropWhileEnd (== slash)

server :: Options -> Snap ()
server options = do
    -- If we're proxied, set the source IP from the X-Forwarded-For header.
    when (oProxied options) ipHeaderFilterSupportingIPv6

    req <- getRequest
    let path = rqContextPath req `BS.append` rqPathInfo req
        method = rqMethod req

    case splitPath path of
      Just components
        | Just what <- parseRequest method components ->
            handleRequest what
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

  httpServe config (server options)
