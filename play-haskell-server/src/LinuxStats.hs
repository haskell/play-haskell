{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -fno-full-laziness -fno-cse #-}
{-# LANGUAGE TypeApplications #-}
module LinuxStats (
  getUptime,
  getLoad,
  getCPUusage,
) where

import Data.IORef
import System.Clock
import System.IO.Unsafe (unsafePerformIO)
import System.IO
import Text.Read (readMaybe)
import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe)


-- | Returns time boot
getUptime :: IO TimeSpec
getUptime = map (readMaybe @Double) . words <$> readFile "/proc/uptime" >>= \case
  Just up : _ ->  return $
    let secs = floor up
    in TimeSpec { sec = secs
                , nsec = round ((up - fromIntegral secs) * s2ns) }
  _ -> return TimeSpec { sec = (-1), nsec = 0 }

-- | The usual exponentially decaying average over 1mins, 5mins, 15mins.
getLoad :: IO (Double, Double, Double)
getLoad = map readMaybe . words <$> readFile "/proc/loadavg" >>= \case
  Just a : Just b : Just c : _ -> return (a, b, c)
  _ -> return (-1, -1, -1)

-- | Returns (percent of CPU used, in this time interval in seconds)
getCPUusage :: IO (Double, TimeSpec)
getCPUusage = do
  (tm, usage) <- readIORef cpuUsageStore
  (tm', usage') <- newCpuUsageValue
  atomicWriteIORef cpuUsageStore (tm', usage')
  let tmdiff = fromIntegral (toNanoSecs (tm' - tm)) / s2ns
  return ((usage' - usage) / tmdiff, tm' - tm)

{-# NOINLINE cpuUsageStore #-}
-- | Stores time of last measurement, and global CPU time until that point
cpuUsageStore :: IORef (TimeSpec, Double)
cpuUsageStore = unsafePerformIO (newCpuUsageValue >>= newIORef)

newCpuUsageValue :: IO (TimeSpec, Double)
newCpuUsageValue = do
  now <- getTime Monotonic
  line <- withFile "/proc/stat" ReadMode hGetLine
  let numbers = dropWhile isSpace (drop 3 line)
      usage = fromMaybe (-1) (readMaybe (takeWhile isDigit numbers)) :: Int
  return (now, fromIntegral usage / 100)
