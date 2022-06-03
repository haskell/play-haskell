{-# LANGUAGE NumericUnderscores #-}
module Snap.Server.Utils.SpamDetect (
  SpamDetect,
  SpamConfig(..),
  initSpamDetect,
  recordCheckSpam,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Int
import qualified System.Clock as Clock
import System.Random (randomRIO)


getTimeSecsMonotonic :: IO Int64
getTimeSecsMonotonic = Clock.sec <$> Clock.getTime Clock.Monotonic

-- | Example values:
--
-- > SpamConfig { spamActionPenalty = {- for you to fill in -}
-- >            , spamHalfTimeSecs = 10
-- >            , spamThreshold = 3.0
-- >            , spamForgetBelowScore = 0.1
-- >            , spamForgetIntervalSecs = 3600 }
data SpamConfig action = SpamConfig
  { spamActionPenalty :: action -> Double
  , spamHalfTimeSecs :: Double
  , spamThreshold :: Double
  , spamForgetBelowScore :: Double
  , spamForgetIntervalSecs :: Int
  }

data SpamDetect action a =
  SpamDetect (SpamConfig action)
             (TVar (Map a (Double, Int64)))

-- | Each time a user performs an action, their spam account is incremented by
-- the corresponding 'actionPenalty'. If an action brings the spam account over
-- 'spamThreshold', the action is marked as spam (but the account is still
-- incremented).
--
-- Over time, a spam account decreases exponentially such that every
-- 'spamHalfTimeSecs' seconds (without intervening actions), the account
-- decreases by half. This is actually implemented by a single O(1) computation
-- every time the actualised account is needed.
--
-- Scores below 'spamForgetBelowScore' are removed from memory; this is done
-- every [1, 1.25] * 'spamForgetIntervalSecs' seconds.
initSpamDetect :: Ord a => SpamConfig action -> IO (SpamDetect action a)
initSpamDetect config = do
  var <- newTVarIO mempty

  void $ forkIO $ forever $ do
    randoffset <- randomRIO (0, spamForgetIntervalSecs config `div` 4)
    threadDelay ((spamForgetIntervalSecs config + randoffset) * 1000_1000)

    now <- getTimeSecsMonotonic
    atomically $ modifyTVar' var (Map.filter (\(score, tm) ->
      progressTime config tm now score >= spamForgetBelowScore config))

  return (SpamDetect config var)

-- | Returns whether the action is considered spam.
recordCheckSpam :: Ord a => action -> SpamDetect action a -> a -> IO Bool
recordCheckSpam action (SpamDetect config var) user = do
  let penalty = spamActionPenalty config action
  now <- getTimeSecsMonotonic
  atomically $ do
    mp <- readTVar var
    let (sc1, tm1) = fromMaybe (0, now) (Map.lookup user mp)
        sc2 = progressTime config tm1 now sc1
    writeTVar var (Map.insert user (sc2 + penalty, now) mp)
    return (sc2 + penalty >= spamThreshold config)

progressTime :: SpamConfig action -> Int64 -> Int64 -> Double -> Double
progressTime config tm1 tm2 sc1 =
  -- If (tm2 - tm1) == halfTimeSecs, then:
  --   sc1 * exp (log 0.5 / h * h) = sc1 * exp (log 0.5) = sc1 * 0.5
  sc1 * exp (log 0.5 / spamHalfTimeSecs config * fromIntegral (tm2 - tm1))
