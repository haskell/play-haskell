{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Statistics where

import Control.Concurrent
import Control.Monad (when)
import Data.IORef
import qualified Data.Time as DT
import qualified Data.Time.Clock.POSIX as DT

import Statistics.Stats


rotateTime :: DT.TimeOfDay
rotateTime = DT.TimeOfDay 5 0 0

data StData = StData
  { stdaJobTime :: Stats ('StatOptions True True True) Double
  , stdaQueueLength :: Stats ('StatOptions True True True) Double }

emptyStData :: StData
emptyStData = StData (statsEmpty 10) (statsEmpty 10)

newtype Statistics = Statistics (IORef StData)

startStatistics :: IO Statistics
startStatistics = do
  ref <- newIORef emptyStData
  _ <- forkIO $ workerThread ref
  return (Statistics ref)

rotateStats :: IORef StData -> IO ()
rotateStats ref = do
  _

workerThread :: IORef StData -> IO ()
workerThread ref = do
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

  when (fromIntegral @Int @Integer maxBound < 24*3600*1000*1000) $ error "This doesn't work on 32-bit systems"

  threadDelay (round (waitTime * 1000*1000))

  rotateStats ref

  workerThread ref
