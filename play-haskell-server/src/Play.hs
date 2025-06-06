{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
import qualified Data.ByteString.Short as BSS
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (secondsToDiffTime)
import qualified Data.Text.Encoding as Enc
import qualified Data.Time.Format as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.LocalTime as Time
import GHC.Generics (Generic)
import Snap.Core hiding (path, method, pass)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, takeFileName, (</>))
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
import Play.Examples
import qualified Play.WorkerPool as WP
import PlayHaskellTypes
import qualified PlayHaskellTypes.Sign as Sign
import qualified PlayHaskellTypes.Statistics as Stats


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

data ClientSubmitReq = ClientSubmitReq
  { csrCode :: Text
  , csrVersion :: Version
  , csrOpt :: Optimisation
  , csrOutput :: Command }
  deriving (Show)

instance J.FromJSON ClientSubmitReq where
  parseJSON (J.Object v) =
    ClientSubmitReq
      <$> v J..: fromString "code"
      <*> v J..: fromString "version"
      <*> (fromMaybe O1 <$> (v J..:? fromString "opt"))
      <*> v J..: fromString "output"
  parseJSON val = J.prependFailure "parsing ClientSubmitReq failed, " (J.typeMismatch "Object" val)

data ClientSaveReq = ClientSaveReq
  { cvrCode :: Text
  , cvrVersion :: Maybe Version
  , cvrOpt :: Maybe Optimisation }
  deriving (Show)

instance J.FromJSON ClientSaveReq where
  parseJSON (J.Object v) =
    ClientSaveReq
      <$> v J..: fromString "code"
      <*> v J..:? fromString "version"
      <*> v J..:? fromString "opt"
  parseJSON val = J.prependFailure "parsing ClientSaveReq failed, " (J.typeMismatch "Object" val)


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

data ViewType = VTPlayground | VTRaw
  deriving (Show)

data WhatRequest
  = Index
  | PostedIndex
  | FromSaved ByteString ViewType
  | Save
  | Versions
  | CurrentChallenge
  | Submit
  | AdminReq AdminReq
  | LegacyRunGHC Command
  | LegacyRedirect ByteString  -- ^ to destination URL
  deriving (Show)

data AdminReq
  = ARDashboard
  | ARStatus
  | ARAddWorker
  | ARRemoveWorker
  | ARRefreshWorker
  deriving (Show)

parseRequest :: Method -> [ByteString] -> Maybe WhatRequest
parseRequest method comps = case (method, comps) of
  (GET, []) -> Just Index
  (POST, []) -> Just PostedIndex
  (GET, ["saved", key]) -> Just (FromSaved key VTPlayground)
  (GET, ["saved", key, "raw"]) -> Just (FromSaved key VTRaw)
  (POST, ["save"]) -> Just Save
  (GET, ["versions"]) -> Just Versions
  (GET, ["challenge"]) -> Just CurrentChallenge
  (POST, ["submit"]) -> Just Submit
  (GET, ["admin"]) -> Just (AdminReq ARDashboard)
  (GET, ["admin", "status"]) -> Just (AdminReq ARStatus)
  (PUT, ["admin", "worker"]) -> Just (AdminReq ARAddWorker)
  (DELETE, ["admin", "worker"]) -> Just (AdminReq ARRemoveWorker)
  (POST, ["admin", "worker", "refresh"]) -> Just (AdminReq ARRefreshWorker)

  (POST, ["compile", "run"]) -> Just (LegacyRunGHC CRun)
  (POST, ["compile", "core"]) -> Just (LegacyRunGHC CCore)
  (POST, ["compile", "asm"]) -> Just (LegacyRunGHC CAsm)
  (GET, ["play"]) -> Just (LegacyRedirect "/")
  (GET, ["play", "paste", key]) -> Just (LegacyRedirect ("/saved/" <> key))
  (GET, ["play", "paste", key, _]) -> Just (LegacyRedirect ("/saved/" <> key))
  _ -> Nothing

handleRequest :: GlobalContext -> Context -> WhatRequest -> Snap ()
handleRequest gctx ctx = \case
  Index -> do
    req <- getRequest
    renderer <- liftIO $ getPageFromGCtx pPlay gctx
    case Map.lookup "code" (rqQueryParams req) of
      Just (source : _) -> writeHTML (renderer Nothing Nothing source Nothing Nothing)
      _ -> do
        snippet <- liftIO randomExampleSnippet
        writeHTML (renderer Nothing Nothing snippet Nothing Nothing)

  PostedIndex -> do
    req <- getRequest
    case Map.lookup "code" (rqPostParams req) of
      Just [source] -> do
        renderer <- liftIO $ getPageFromGCtx pPlay gctx
        writeHTML (renderer Nothing Nothing source Nothing Nothing)
      _ ->
        httpError 400 "Invalid request"

  FromSaved key vt -> do
    res <- liftIO $ DB.getPaste (gcDb gctx) key
    case res of
      Just (_, Contents [] _ _ _ _) -> do
        modifyResponse (setContentType (Char8.pack "text/plain")
                        . setResponseCode 404)
        writeBS (Char8.pack "Save key not found (empty file list?)")

      Just (mmoddate, Contents ((_, source) : _) _ _ mghcver mghcopt) ->
        case vt of
          VTPlayground -> do
            renderer <- liftIO $ getPageFromGCtx pPlay gctx
            writeHTML (renderer (Just key) mmoddate source mghcver mghcopt)
          VTRaw -> do
            liftIO $ print mmoddate
            let gmtTimeZone = Time.TimeZone 0 False "GMT"
            modifyResponse
              (setContentType (Char8.pack "text/plain")
               . (case mmoddate of
                    Nothing -> id
                    Just moddate ->
                      let s = Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat
                                (Time.utcToZonedTime gmtTimeZone (Time.posixSecondsToUTCTime moddate))
                      in setHeader "Last-Modified" (Char8.pack s)))
            writeBS source

      Nothing -> do
        modifyResponse (setContentType (Char8.pack "text/plain")
                        . setResponseCode 404)
        writeBS (Char8.pack "Save key not found")

  Save -> execExitEarlyT $ do
    postdata <- getRequestBodyEarlyExit 1000_000 "Program too large"
    cvr <-
      case J.decodeStrict' postdata of
        Just request -> return request
        _ -> do lift (httpError 400 "Invalid JSON")
                exitEarly ()

    let code = Enc.encodeUtf8 (cvrCode cvr)
    when (BS.length code > maxSaveFileSize) $ do
      lift (httpError 400 "File too large")
      exitEarly ()

    let contents = Contents [(Nothing, code)] Nothing Nothing (cvrVersion cvr) (cvrOpt cvr)

    req <- lift getRequest
    let srcip = Char8.unpack (rqClientAddr req)

    mkey <- liftIO $ genStorePaste gctx (ctxRNG ctx) srcip contents
    lift $ case mkey of
      Right key -> do
        modifyResponse (setContentType (Char8.pack "text/plain"))
        writeBS key
      Left err -> httpError 500 err

  Versions -> do
    modifyResponse (setContentType (Char8.pack "text/plain"))
    versions <- liftIO (WP.getAvailableVersions (ctxPool ctx))
    writeJSON versions

  -- TODO: remove this. This is present only to let the upgrade to /submit go a bit more smoothly.
  CurrentChallenge -> do
    modifyResponse (setContentType (Char8.pack "text/plain"))
    key <- liftIO $ servingChallenge (ctxChallengeKey ctx)
    writeText key

  -- Open a local exit-early block instead of using Snap's early-exit
  -- functionality, because this is more local.
  Submit -> execExitEarlyT $ do
    postdata <- getRequestBodyEarlyExit 1000_000 "Program too large"

    csr <-
      case J.decodeStrict' postdata of
        Just request -> return request
        _ -> do lift (httpError 400 "Invalid JSON")
                exitEarly ()

    handleSubmitRequest csr

  -- Open a local exit-early block instead of using Snap's early-exit
  -- functionality, because this is more local.
  -- TODO: remove this. This is present only to let the upgrade to /submit go a bit more smoothly. Then also remove /challenge.
  LegacyRunGHC runner -> execExitEarlyT $ do
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

    handleSubmitRequest
      ClientSubmitReq { csrCode = source
                      , csrVersion = version
                      , csrOpt = opt
                      , csrOutput = runner }

  AdminReq adminreq -> do
    getBasicAuthCredentials <$> getRequest >>= \case
      Just (user, pass)
        | user == "admin"
        , Just pass == gcAdminPassword gctx
        -> handleAdminRequest ctx adminreq
      _ -> modifyResponse (requireBasicAuth "admin")

  LegacyRedirect url -> redirect' url 301  -- moved permanently
  where
    handleSubmitRequest :: ClientSubmitReq -> ExitEarlyT () Snap ()
    handleSubmitRequest ClientSubmitReq {csrCode=source, csrVersion=version, csrOpt=opt, csrOutput=submitType} = do
      let runreq = RunRequest { runreqCommand = submitType
                              , runreqSource = source
                              , runreqVersion = version
                              , runreqOpt = opt }

      (mresult, numqueued) <- liftIO $ WP.submitJob (ctxPool ctx) runreq
      result <- case mresult of
        Just r -> do
          liftIO $ forM_ (gcStatistics gctx) $ \stats -> do
            let rectime = case r of
                            RunResponseErr err -> Left err
                            RunResponseOk {runresTimeTakenSecs = secs} -> Right secs
            Stats.recordJob stats (Just rectime) numqueued
          return r
        Nothing -> do
          liftIO $ forM_ (gcStatistics gctx) $ \stats -> do
            Stats.recordJob stats Nothing numqueued
          lift (httpError 503 "Service busy, please try again later")
          exitEarly ()

      lift $ writeJSON result

data AddWorkerRequest = AddWorkerRequest
  { awreqHostname :: String
  , awreqPubkey :: String }
  deriving (Show, Generic)

instance J.FromJSON AddWorkerRequest where
  parseJSON = J.genericParseJSON J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop 5 }

data RemoveWorkerRequest = RemoveWorkerRequest
  { rmwreqHostname :: String }
  deriving (Show, Generic)

instance J.FromJSON RemoveWorkerRequest where
  parseJSON = J.genericParseJSON J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop 6 }

data RefreshWorkerRequest = RefreshWorkerRequest
  { rfwreqHostname :: String }
  deriving (Show, Generic)

instance J.FromJSON RefreshWorkerRequest where
  parseJSON = J.genericParseJSON J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop 6 }

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

  ARRefreshWorker -> execExitEarlyT $ do
    RefreshWorkerRequest host <- getRequestBodyEarlyExitJSON 1024 "request too large"

    liftIO $ WP.refreshWorker (ctxPool ctx) (Char8.pack host)
    lift $ putResponse $ setResponseCode 200 emptyResponse

playModule :: IO ServerModule
playModule = do
  let aceDir = "ace-builds/src-min-noconflict"
  aceFiles <- map (\path -> StaticFile (aceDir </> path) ["ace-files", Char8.pack (takeFileName path)] "text/javascript")
              . filter (\path -> takeExtension path == ".js")
              <$> listDirectory ("static" </> aceDir)

  return $ ServerModule
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
    , smStaticFiles = StaticFile "play-index.js" ["play-index.js"] "text/javascript"
                      : StaticFile "haskell-logo-tw.svg" ["haskell-logo-tw.svg"] "image/svg+xml"
                      : StaticFile "haskell-play-logo.png" ["haskell-play-logo.png"] "image/png"
                      : aceFiles
    }
