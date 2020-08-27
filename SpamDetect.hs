module SpamDetect (
    SpamDetect,
    initSpamDetect,
    recordCheckSpam
) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Int
import qualified System.Clock as Clock

import RecentList (RecentList)
import qualified RecentList as RL


maxNumActions :: Int
recordSecs :: Int64
(maxNumActions, recordSecs) = (5, 60)

getTimeSecsMonotonic :: IO Int64
getTimeSecsMonotonic = Clock.sec <$> Clock.getTime Clock.Monotonic

newtype SpamDetect a = SpamDetect (TVar (Map a (RecentList Int64)))

initSpamDetect :: Ord a => IO (SpamDetect a)
initSpamDetect = do
    var <- newTVarIO mempty
    return (SpamDetect var)

-- Returns whether the action is considered spam
recordCheckSpam :: Ord a => SpamDetect a -> a -> IO Bool
recordCheckSpam (SpamDetect var) user = do
    now <- getTimeSecsMonotonic
    atomically $ do
        mp <- readTVar var
        case Map.lookup user mp of
            Nothing -> do
                rl <- RL.new (replicate maxNumActions 0)  -- 0 is old
                RL.add rl now
                writeTVar var (Map.insert user rl mp)
                return False
            Just rl -> do
                firstTime <- RL.get rl 0
                RL.add rl now
                return $ now - firstTime <= recordSecs
