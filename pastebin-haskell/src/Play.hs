{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Play (playModule) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (secondsToDiffTime)
import Snap.Core hiding (path, method)
import Text.Read (readMaybe)

import Pages
import Paste.DB (getPaste, Contents(..))
import ServerModule
import Snap.Server.Utils
import Snap.Server.Utils.Challenge
import Snap.Server.Utils.ExitEarly
import Snap.Server.Utils.SpamDetect
import qualified Play.WorkerPool as WP
import PlayHaskellTypes
import PlayHaskellTypes.Constants


data ClientJobReq = ClientJobReq
  { cjrGivenKey :: Text
  , cjrSource :: Text
  , cjrVersion :: Version
  , cjrOpt :: Optimisation }
  deriving (Show)

instance J.FromJSON ClientJobReq where
  parseJSON (J.Object v) =
    ClientJobReq <$> v J..: fromString "challenge"
                 <*> v J..: fromString "source"
                 <*> v J..: fromString "version"
                 <*> (fromMaybe O1 <$> (v J..:? fromString "opt"))
  parseJSON val = J.prependFailure "parsing ClientJobReq failed, " (J.typeMismatch "Object" val)


data Context = Context WP.WPool ChallengeKey

data WhatRequest
  = Index
  | FromPaste ByteString Int
  | Versions
  | CurrentChallenge
  | RunGHC Command
  deriving (Show)

parseRequest :: Method -> [ByteString] -> Maybe WhatRequest
parseRequest method comps = case (method, comps) of
  (GET, ["play"]) -> Just Index
  (GET, ["play", "paste", key]) -> Just (FromPaste key 1)
  (GET, ["play", "paste", key, idxs])
    | Just idx <- readMaybe (Char8.unpack idxs) -> Just (FromPaste key idx)
  (GET, ["play", "versions"]) -> Just Versions
  (GET, ["play", "challenge"]) -> Just CurrentChallenge
  (POST, ["play", "run"]) -> Just (RunGHC CRun)
  (POST, ["play", "core"]) -> Just (RunGHC CCore)
  (POST, ["play", "asm"]) -> Just (RunGHC CAsm)
  _ -> Nothing

handleRequest :: GlobalContext -> Context -> WhatRequest -> Snap ()
handleRequest gctx (Context pool challenge) = \case
  Index -> do
    renderer <- liftIO $ getPageFromGCtx pPlay gctx
    writeHTML (renderer Nothing)

  FromPaste key idx -> do
    res <- liftIO $ getPaste (gcDb gctx) key
    let buildPage contents = do
          renderer <- liftIO $ getPageFromGCtx pPlay gctx
          writeHTML (renderer (Just contents))
    case res of
      Just (_, Contents [] _ _) -> do
        modifyResponse (setContentType (Char8.pack "text/plain"))
        writeBS (Char8.pack "That paste seems to have no files?")

      Just (_, Contents l _ _)
        | idx >= 1
        , (_, source) : _ <- drop (idx - 1) l ->
            buildPage source

      Just (_, Contents _ _ _) -> do
        modifyResponse (setContentType (Char8.pack "text/plain"))
        writeBS (Char8.pack "File index out of range")
      Nothing -> do
        modifyResponse (setContentType (Char8.pack "text/plain"))
        writeBS (Char8.pack "That paste does not exist!")

  Versions -> do
    modifyResponse (setContentType (Char8.pack "text/plain"))
    versions <- liftIO (WP.getAvailableVersions pool)
    writeJSON versions

  CurrentChallenge -> do
    modifyResponse (setContentType (Char8.pack "text/plain"))
    key <- liftIO $ servingChallenge challenge
    writeText key

  -- Open a local exit-early block instead of using Snap's early-exit
  -- functionality, because this is more local.
  RunGHC runner -> execExitEarlyT $ do
    req <- lift getRequest
    isSpam <- liftIO $ recordCheckSpam PlayRunStart (gcSpam gctx) (rqClientAddr req)
    when isSpam $ do
      lift (httpError 429 "Please slow down a bit, you're rate limited")
      exitEarly ()

    postdata <- getRequestBodyEarlyExit 1000_000 "Program too large"

    ClientJobReq {cjrGivenKey=givenKey, cjrSource=source, cjrVersion=version, cjrOpt=opt} <-
      case J.decodeStrict' postdata of
        Just request -> return request
        _ -> do lift (httpError 400 "Invalid JSON")
                exitEarly ()

    liftIO (checkChallenge challenge givenKey) >>= \case
      True -> return ()
      False -> do lift (httpError 400 "Invalid challenge, request again")
                  exitEarly ()

    let runreq = RunRequest { runreqCommand = runner
                            , runreqSource = source
                            , runreqVersion = version
                            , runreqOpt = opt }

    mresult <- liftIO $ WP.submitJob pool runreq
    result <- case mresult of
      Just r -> return r
      Nothing -> do lift (httpError 503 "Service busy, please try again later")
                    exitEarly ()

    -- Record the run as a spam-checking action, but don't actually act
    -- on the return value yet; that will come on the next user action
    let timeoutSecs = fromIntegral runTimeoutMicrosecs / 1e6
        timeTakenSecs = case result of
          RunResponseErr RETimeOut -> timeoutSecs
          RunResponseErr REBackend -> timeoutSecs / 6  -- shrug
          RunResponseOk{} -> runresTimeTakenSecs result
        timeFraction = timeTakenSecs / timeoutSecs
    _ <- liftIO $ recordCheckSpam (PlayRunTimeoutFraction timeFraction) (gcSpam gctx) (rqClientAddr req)

    lift $ writeJSON result

playModule :: ServerModule
playModule = ServerModule
  { smMakeContext = \gctx _options k -> do
      -- TODO: the max queue length is a completely arbitrary value
      pool <- WP.newPool (gcServerSecretKey gctx) 10
      challenge <- makeRefreshingChallenge (secondsToDiffTime (24 * 3600))
      k (Context pool challenge)
  , smParseRequest = parseRequest
  , smHandleRequest = handleRequest
  , smStaticFiles = [("bundle.js", "text/javascript")
                    ,("haskell-logo-tw.svg", "image/svg+xml")]
  }
