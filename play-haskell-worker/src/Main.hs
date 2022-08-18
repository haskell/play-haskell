{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Char (ord, isSpace)
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Snap.Core hiding (path, method)
import Snap.Http.Server
import System.Exit
import System.IO

import PlayHaskellTypes
import PlayHaskellTypes.Sign (PublicKey, SecretKey)
import qualified PlayHaskellTypes.Sign as Sign
import Snap.Server.Utils
import Snap.Server.Utils.ExitEarly
import Snap.Server.Utils.Hex
import qualified Snap.Server.Utils.Options as Opt
import Snap.Server.Utils.Shim

import GHCPool


data Context = Context
  { ctxPool :: Pool
  , ctxSecretKey :: SecretKey
  , ctxTrustedServers :: [PublicKey] }

data WhatRequest
  = SubmitJob
  | Health
  deriving (Show)

parseRequest :: Method -> [ByteString] -> Maybe WhatRequest
parseRequest method comps = case (method, comps) of
  (POST, ["job"]) -> Just SubmitJob
  (GET, ["health"]) -> Just Health
  _ -> Nothing

handleRequest :: Context -> WhatRequest -> Snap ()
handleRequest ctx = \case
  SubmitJob -> execExitEarlyT $ do
    msg <- getRequestBodyEarlyExitJSON 1000_000 "Program too large"

    when (sesmsgPublicKey msg `notElem` ctxTrustedServers ctx) $ do
      liftIO $ putStrLn $ "Got pkey " ++ show (sesmsgPublicKey msg)
      liftIO $ putStrLn $ "Trusted list: " ++ show (ctxTrustedServers ctx)
      lift $ httpError 401 "Public key not in trusted list"
      exitEarly ()

    let runreq = sesmsgContent msg

    when (not (Sign.verify (sesmsgPublicKey msg) (signingBytes runreq) (sesmsgSignature msg))) $ do
      lift $ httpError 400 "Invalid signature"
      exitEarly ()

    result <- liftIO $ runInPool (ctxPool ctx)
                (runreqCommand runreq)
                (runreqVersion runreq)
                (runreqOpt runreq)
                (runreqSource runreq)
    let response = case result of
          Left err -> RunResponseErr err
          Right res -> RunResponseOk
                         { runresExitCode = resExitCode res
                         , runresGhcOut = resGhcOut res
                         , runresStdout = resStdout res
                         , runresStderr = resStderr res
                         , runresTimeTakenSecs = resTimeTaken res }
    
    lift $ writeJSON (signMessage (ctxSecretKey ctx) response)

  Health -> do
    response <- HealthResponse <$> (map Version <$> liftIO availableVersions)
                               <*> pure (poolParallelism (ctxPool ctx))
    writeJSON (signMessage (ctxSecretKey ctx) response)

splitPath :: ByteString -> Maybe [ByteString]
splitPath path
  | BS.null path || BS.head path /= fromIntegral (ord '/')
  = Nothing
splitPath path = Just (BS.split (fromIntegral (ord '/')) (trimSlashes path))
  where
    trimSlashes :: ByteString -> ByteString
    trimSlashes = let slash = fromIntegral (ord '/')
                  in BS.dropWhile (== slash) . BS.dropWhileEnd (== slash)

server :: Options -> Context -> Snap ()
server options ctx = do
  -- If we're proxied, set the source IP from the X-Forwarded-For header.
  when (oProxied options) ipHeaderFilterSupportingIPv6

  req <- getRequest
  let path = rqContextPath req `BS.append` rqPathInfo req
      method = rqMethod req

  case splitPath path of
    Just components
      | Just what <- parseRequest method components ->
          handleRequest ctx what
      | otherwise ->
          httpError 404 "Path not found"
    Nothing -> httpError 400 "Invalid URL"

config :: Int -> Config Snap a
config port =
  let stderrlogger = ConfigIoLog (Char8.hPutStrLn stderr)
  in setAccessLog stderrlogger
     . setErrorLog stderrlogger
     . setPort port
     $ defaultConfig

data Options = Options { oProxied :: Bool
                       , oSecKeyFile :: FilePath
                       , oTrustedKeys :: FilePath
                       , oPort :: Int }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options False "" "" 8124

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  options <- Opt.parseOptions $ Opt.Interface defaultOptions $ Map.fromList
    [("--proxied", Opt.Flag
        "Assumes the server is running behind a proxy that sets \
        \X-Forwarded-For, instead of using the source IP of a \
        \request for rate limiting."
        (\o -> o { oProxied = True }))
    ,("--secretkey", Opt.Setter
        "Required. Path to file that contains the secret key \
        \of this worker. The file should contain 32 random bytes \
        \in hexadecimal notation."
        (\o s -> o { oSecKeyFile = s }))
    ,("--trustedkeys", Opt.Setter
        "Required. Path to file that contains the public keys of \
        \play-haskell servers that this worker trusts. The \
        \file should contain a number of hexadecimal strings, \
        \each encoding a public key of length 32 bytes, separated \
        \by spaces or newlines."
        (\o s -> o { oTrustedKeys = s }))
    ,("--port", Opt.Setter
        "Port to listen on for http connections."
        (\o s -> o { oPort = case readMaybe s of Just n -> n ; Nothing -> error "Invalid --port value" }))
    ,("--help", Opt.Help)
    ,("-h", Opt.Help)]

  when (oSecKeyFile options == "") $ die "'--secretkey' is required"
  skey <- (hexDecodeBS . trim >=> Sign.readSecretKey) <$> readFile (oSecKeyFile options) >>= \case
            Nothing -> die "Secret key file contains invalid key"
            Just skey -> return skey

  when (oTrustedKeys options == "") $ die "'--trustedkeys' is required"
  pkeys <- mapM (hexDecodeBS >=> Sign.readPublicKey) . words <$> readFile (oTrustedKeys options) >>= \case
             Nothing -> die "Trusted public keys file of invalid format"
             Just pkeys -> return pkeys

  let Sign.PublicKey ownPkey = Sign.publicKey skey
  putStrLn $ "My public key: " ++ hexEncode ownPkey

  nprocs <- getNumCapabilities
  putStrLn $ "Starting worker with " ++ show nprocs ++ " threads (use +RTS -N<n> to set)"
  pool <- makePool nprocs
  let ctx = Context pool skey pkeys

  httpServe (config (oPort options)) (server options ctx)
