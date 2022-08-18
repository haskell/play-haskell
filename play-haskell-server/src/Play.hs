{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Play (playModule) where

import Control.Concurrent.STM
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (secondsToDiffTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Snap.Core hiding (path, method, pass)
import System.Random (StdGen, genByteString, newStdGen)

import DB (KeyType, Contents(..), ClientAddr)
import qualified DB
import Pages
import ServerModule
import Snap.Server.Utils
import Snap.Server.Utils.BasicAuth
import Snap.Server.Utils.Challenge
import Snap.Server.Utils.ExitEarly
import Snap.Server.Utils.Hex
import Snap.Server.Utils.SpamDetect
import qualified Play.WorkerPool as WP
import PlayHaskellTypes
import PlayHaskellTypes.Constants
import qualified PlayHaskellTypes.Sign as Sign


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


saveKeyLength :: Int
saveKeyLength = 8

maxSaveFileSize :: Int
maxSaveFileSize = 128 * 1024

genKey :: StdGen -> (KeyType, StdGen)
genKey gen =
  let (bs, gen') = genByteString saveKeyLength gen
      intoAlphabet n = BS.index alphabet (fromIntegral n `rem` BS.length alphabet)
  in (BS.map intoAlphabet bs, gen')
  where
    alphabet :: ByteString
    alphabet = Char8.pack (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

genKey' :: TVar StdGen -> IO KeyType
genKey' var = atomically $ do
  gen <- readTVar var
  let (key, gen') = genKey gen
  writeTVar var gen'
  return key

-- returns the generated key, or an error string
genStorePaste :: GlobalContext -> TVar StdGen -> ClientAddr -> Contents -> IO (Either String KeyType)
genStorePaste gctx stvar srcip contents =
  let loop iter = do
        key <- genKey' stvar
        DB.storePaste (gcDb gctx) srcip key contents >>= \case
          Nothing -> return (Right key)
          Just DB.ErrExists
              | iter < 5 -> loop (iter + 1)  -- try again with a new key
              | otherwise -> return (Left "Database full?")
          Just DB.ErrFull ->
            return (Left "Too many snippets saved, saving is temporarily disabled")
  in loop (0 :: Int)


data Context = Context
  { ctxPool :: WP.WPool
  , ctxChallengeKey :: ChallengeKey
  , ctxRNG :: TVar StdGen }

data WhatRequest
  = Index
  | FromSaved ByteString
  | Save
  | Versions
  | CurrentChallenge
  | RunGHC Command
  | AdminReq AdminReq
  | LegacyRedirect ByteString  -- ^ to destination URL
  deriving (Show)

data AdminReq
  = ARDashboard
  | ARStatus
  | ARAddWorker
  | ARRemoveWorker
  deriving (Show)

parseRequest :: Method -> [ByteString] -> Maybe WhatRequest
parseRequest method comps = case (method, comps) of
  (GET, []) -> Just Index
  (GET, ["saved", key]) -> Just (FromSaved key)
  (POST, ["save"]) -> Just Save
  (GET, ["versions"]) -> Just Versions
  (GET, ["challenge"]) -> Just CurrentChallenge
  (POST, ["compile", "run"]) -> Just (RunGHC CRun)
  (POST, ["compile", "core"]) -> Just (RunGHC CCore)
  (POST, ["compile", "asm"]) -> Just (RunGHC CAsm)
  (GET, ["admin"]) -> Just (AdminReq ARDashboard)
  (GET, ["admin", "status"]) -> Just (AdminReq ARStatus)
  (PUT, ["admin", "worker"]) -> Just (AdminReq ARAddWorker)
  (DELETE, ["admin", "worker"]) -> Just (AdminReq ARRemoveWorker)

  (GET, ["play"]) -> Just (LegacyRedirect "/")
  (GET, ["play", "paste", key]) -> Just (LegacyRedirect ("/saved/" <> key))
  (GET, ["play", "paste", key, _]) -> Just (LegacyRedirect ("/saved/" <> key))
  _ -> Nothing

handleRequest :: GlobalContext -> Context -> WhatRequest -> Snap ()
handleRequest gctx ctx = \case
  Index -> do
    renderer <- liftIO $ getPageFromGCtx pPlay gctx
    writeHTML (renderer Nothing)

  FromSaved key -> do
    res <- liftIO $ DB.getPaste (gcDb gctx) key
    let buildPage contents = do
          renderer <- liftIO $ getPageFromGCtx pPlay gctx
          writeHTML (renderer (Just contents))
    case res of
      Just (_, Contents [] _ _) -> do
        modifyResponse (setContentType (Char8.pack "text/plain"))
        writeBS (Char8.pack "Save key not found (empty file list?)")

      Just (_, Contents ((_, source) : _) _ _) ->
        buildPage source

      Nothing -> do
        modifyResponse (setContentType (Char8.pack "text/plain"))
        writeBS (Char8.pack "Save key not found")

  Save -> do
    req <- getRequest
    isSpam <- liftIO $ recordCheckSpam PlaySave (gcSpam gctx) (rqClientAddr req)
    if isSpam
      then httpError 429 "Please slow down a bit, you're rate limited"
      else do body <- readRequestBody (fromIntegral @Int @Word64 maxSaveFileSize)
              let body' = BSL.toStrict body
              let contents = Contents [(Nothing, body')] Nothing Nothing
                  srcip = Char8.unpack (rqClientAddr req)
              mkey <- liftIO $ genStorePaste gctx (ctxRNG ctx) srcip contents
              case mkey of
                Right key -> do
                  modifyResponse (setContentType (Char8.pack "text/plain"))
                  writeBS key
                Left err -> httpError 500 err

  Versions -> do
    modifyResponse (setContentType (Char8.pack "text/plain"))
    versions <- liftIO (WP.getAvailableVersions (ctxPool ctx))
    writeJSON versions

  CurrentChallenge -> do
    modifyResponse (setContentType (Char8.pack "text/plain"))
    key <- liftIO $ servingChallenge (ctxChallengeKey ctx)
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

    liftIO (checkChallenge (ctxChallengeKey ctx) givenKey) >>= \case
      True -> return ()
      False -> do lift (httpError 400 "Invalid challenge, request again")
                  exitEarly ()

    let runreq = RunRequest { runreqCommand = runner
                            , runreqSource = source
                            , runreqVersion = version
                            , runreqOpt = opt }

    mresult <- liftIO $ WP.submitJob (ctxPool ctx) runreq
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

  AdminReq adminreq -> do
    getBasicAuthCredentials <$> getRequest >>= \case
      Just (user, pass)
        | user == "admin"
        , Just pass == gcAdminPassword gctx
        -> handleAdminRequest ctx adminreq
      _ -> modifyResponse (requireBasicAuth "admin")

  LegacyRedirect url -> redirect' url 301  -- moved permanently

data AddWorkerRequest = AddWorkerRequest
  { awreqHostname :: String
  , awreqPubkey :: String }
  deriving (Show, Generic)

instance J.FromJSON AddWorkerRequest where
  parseJSON = J.genericParseJSON J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop 5 }

data RemoveWorkerRequest = RemoveWorkerRequest
  { rwreqHostname :: String }
  deriving (Show, Generic)

instance J.FromJSON RemoveWorkerRequest where
  parseJSON = J.genericParseJSON J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop 5 }

handleAdminRequest :: Context -> AdminReq -> Snap ()
handleAdminRequest ctx = \case
  ARDashboard -> do
    modifyResponse (setContentType (Char8.pack "text/html"))
    sendFile "static/admin_dashboard.html"

  ARStatus -> do
    status <- liftIO $ WP.getPoolStatus (ctxPool ctx)
    writeJSON status

  ARAddWorker -> execExitEarlyT $ do
    AddWorkerRequest host pkeyhex <- getRequestBodyEarlyExitJSON 1024 "request too large"
    when (any (\b -> b <= chr 32 && b >= chr 127) (host ++ pkeyhex)) $ do
      lift $ httpError 400 "Non-printable input"
      exitEarly ()

    pkey <- case Sign.readPublicKey . BSS.fromShort =<< hexDecode pkeyhex of
              Just res -> return res
              _ -> do lift $ httpError 400 "Invalid pubkey (must be 64 hex digits)"
                      exitEarly ()

    liftIO $ WP.addWorker (ctxPool ctx) (Char8.pack host) pkey
    lift $ putResponse $ setResponseCode 200 emptyResponse

  ARRemoveWorker -> execExitEarlyT $ do
    RemoveWorkerRequest host <- getRequestBodyEarlyExitJSON 1024 "request too large"

    liftIO $ WP.removeWorker (ctxPool ctx) (Char8.pack host)
    lift $ putResponse $ setResponseCode 200 emptyResponse

playModule :: ServerModule
playModule = ServerModule
  { smMakeContext = \gctx _options k -> do
      -- TODO: the max queue length is a completely arbitrary value
      pool <- WP.newPool (gcServerSecretKey gctx) 10
      challenge <- makeRefreshingChallenge (secondsToDiffTime (24 * 3600))
      rng <- newStdGen >>= newTVarIO

      forM_ (gcPreloadWorkers gctx) $ \(host, pubkey) ->
        WP.addWorker pool host pubkey

      k (Context { ctxPool = pool
                 , ctxChallengeKey = challenge
                 , ctxRNG = rng })
  , smParseRequest = parseRequest
  , smHandleRequest = handleRequest
  , smStaticFiles = [("bundle.js", "text/javascript")
                    ,("haskell-logo-tw.svg", "image/svg+xml")
                    ,("haskell-play-logo.png", "image/png")]
  }
