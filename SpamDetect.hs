{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module SpamDetect (
    Action(..),
    SpamDetect,
    initSpamDetect,
    recordCheckSpam
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



-- CONSTANTS

data Action = Post
  deriving (Show)

actionPenalty :: Action -> Float
actionPenalty Post = 1.4

halfTimeSecs :: Float
halfTimeSecs = 10

spamThreshold :: Float
spamThreshold = 3.0

forgetBelowScore :: Float
forgetBelowScore = 0.1

forgetIntervalSecs :: Int
forgetIntervalSecs = 3600

forgetIntervalFuzzSecs :: Int
forgetIntervalFuzzSecs = 120


-- IMPLEMENTATION

getTimeSecsMonotonic :: IO Int64
getTimeSecsMonotonic = Clock.sec <$> Clock.getTime Clock.Monotonic

newtype SpamDetect a = SpamDetect (TVar (Map a (Float, Int64)))

initSpamDetect :: Ord a => IO (SpamDetect a)
initSpamDetect = do
    var <- newTVarIO mempty

    void $ forkIO $ forever $ do
        randoffset <- randomRIO (0, forgetIntervalFuzzSecs)
        threadDelay (forgetIntervalSecs * 1000 * 1000 + randoffset)
        forgetOld var

    return (SpamDetect var)

-- Returns whether the action is considered spam
recordCheckSpam :: Ord a => Action -> SpamDetect a -> a -> IO Bool
recordCheckSpam (actionPenalty -> penalty) (SpamDetect var) user = do
    now <- getTimeSecsMonotonic
    atomically $ do
        mp <- readTVar var
        let (sc1, tm1) = fromMaybe (0, now) (Map.lookup user mp)
            sc2 = progressTime tm1 now sc1
        writeTVar var (Map.insert user (sc2 + penalty, now) mp)
        return (sc2 + penalty >= spamThreshold)

forgetOld :: Ord a => TVar (Map a (Float, Int64)) -> IO ()
forgetOld var = do
    now <- getTimeSecsMonotonic
    atomically $ modifyTVar' var (Map.filter (\(score, tm) ->
        progressTime tm now score >= forgetBelowScore))

progressTime :: Int64 -> Int64 -> Float -> Float
progressTime tm1 tm2 sc1 =
    -- If (tm2 - tm1) == halfTimeSecs, then:
    --   sc1 * exp (log 0.5 / h * h) = sc1 * exp (log 0.5) = sc1 * 0.5
    sc1 * exp (log 0.5 / halfTimeSecs * fromIntegral (tm2 - tm1))
