{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module PlayHaskellTypes.Statistics (
  Statistics,
  startStatistics,
  recordJob,
) where

import Control.Concurrent
import Control.Exception (handle)
import Control.Monad (when)
import qualified Data.Aeson as J
import qualified Data.Base64.Types as Base64
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.String (fromString)
import qualified Data.Time as DT
import qualified Data.Time.Clock.POSIX as DT
import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Client.TLS as N
import qualified Network.HTTP.Types.Header as N
import qualified Network.HTTP.Types.Method as N
import qualified Network.HTTP.Types.Status as N
import System.IO (hPutStrLn, stderr)

import PlayHaskellTypes
import PlayHaskellTypes.Statistics.Stats


rotateTime :: DT.TimeOfDay
rotateTime = DT.TimeOfDay 5 0 0

data StData = StData
  { stdaJobTime :: (Stats Double, Histogram Double)
  , stdaQueueLength :: (Stats Double, Histogram Double)  -- really an integer histogram
  , stdaBusyRejects :: Int
  , stdaErrors :: Map RunError Int
  }

emptyStData :: StData
emptyStData = StData (mempty, histEmpty 0 6 12)  -- job timeout is 5 sec
                     (mempty, histEmpty 0 11 11)  -- max queue length is 10; needs to be a column for 10
                     0
                     mempty

data Statistics = Statistics N.Manager ByteString (IORef StData)

-- | Argument: statusbot password.
startStatistics :: ByteString -> IO Statistics
startStatistics statusbotpass = do
  ref <- newIORef emptyStData
  mgr <- N.newTlsManager
  let statistics = Statistics mgr statusbotpass ref
  _ <- forkIO $ workerThread statistics
  return statistics

-- Arguments:
-- * Just Left: Error returned from the worker;
--   Just Right: Time taken for the job on the worker;
--   Nothing: Rejected because queue was full
-- * Job queue length upon submission of the job
recordJob :: Statistics -> Maybe (Either RunError Double) -> Int -> IO ()
recordJob (Statistics _ _ ref) mtime queuelen = do
  let add (StData (jts, jth) (qls, qlh) nrejects nerrs) =
        StData (case mtime of Just (Right time) -> (statsSingleton time <> jts, histAdd time jth)
                              _ -> (jts, jth))
               (statsSingleton (fromIntegral queuelen) <> qls, histAdd (fromIntegral queuelen) qlh)
               (nrejects + fromEnum (isNothing mtime))
               (case mtime of Just (Left err) -> Map.insertWith (+) err 1 nerrs
                              _ -> nerrs)
  atomicModifyIORef' ref (\stda -> (add stda, ()))

rotateStats :: Statistics -> IO ()
rotateStats (Statistics mgr statusbotpass ref) = do
  stda <- atomicModifyIORef' ref (\stda -> (emptyStData, stda))
  let msgPart label (stats, hist) = label ++ ": " ++ prettyStats stats ++ " (" ++ histPretty hist ++ ")"
      message = msgPart "Job time" (stdaJobTime stda) ++ " | " ++
                msgPart "Queue len" (stdaQueueLength stda) ++
                (case stdaBusyRejects stda of
                   0 -> ""
                   n -> " (" ++ show n ++ " rejects)") ++
                (case Map.assocs (stdaErrors stda) of
                   [] -> ""
                   l -> " (errs: " ++ intercalate ", " [show e ++ ": " ++ show n | (e, n) <- l] ++ ")")
  hPutStrLn stderr message
  _ <- forkIO $
    let body = J.object [(fromString "sender", J.String (fromString "play-haskell stats"))
                        ,(fromString "text", J.String (fromString message))]
        auth = BS8.pack "Basic "
               <> Base64.extractBase64 (Base64.encodeBase64' (BS8.pack "play-haskell:" <> statusbotpass))
        nreq = N.defaultRequest { N.host = BS8.pack "tomsmeding.com"
                                , N.port = 443
                                , N.secure = True
                                , N.method = N.methodPost
                                , N.path = BS8.pack "/statusbot"
                                , N.requestBody = N.RequestBodyLBS (J.encode body)
                                , N.requestHeaders = [(N.hContentType, BS8.pack "application/json")
                                                     ,(N.hAuthorization, auth)] }
    in handle (\(e :: N.HttpException) -> print e) $
       N.withResponse nreq mgr $ \response -> do
         when (N.statusCode (N.responseStatus response) /= 200) $
           hPutStrLn stderr $ "Failed to submit statusbot notification: code "
                              ++ show (N.responseStatus response)
  return ()

workerThread :: Statistics -> IO ()
workerThread statistics = do
  now <- DT.getCurrentTime
  zone <- DT.getTimeZone now
  let DT.LocalTime day _timeofday = DT.utcToLocalTime zone now
  let waitTill =
        let candidate@(DT.UTCTime utcday utctime) =
              DT.localTimeToUTC zone (DT.LocalTime day rotateTime)
        in if candidate <= now then DT.UTCTime (succ utcday) utctime
                               else candidate

  -- converting to posix time here is not quite correct, but I don't care
  let waitTime = realToFrac (DT.utcTimeToPOSIXSeconds waitTill) - realToFrac (DT.utcTimeToPOSIXSeconds now) :: Double

  hPutStrLn stderr $ "[statistics] waiting " ++ show waitTime ++ " seconds before rotate"

  when (fromIntegral @Int @Integer maxBound < 24*3600*1000*1000) $ error "This doesn't work on 32-bit systems"
  threadDelay (round (waitTime * 1000*1000))

  rotateStats statistics

  workerThread statistics
