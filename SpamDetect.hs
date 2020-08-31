{-# LANGUAGE TupleSections #-}
module SpamDetect (
    SpamDetect,
    initSpamDetect,
    recordCheckSpam
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM, when, void)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Int
import qualified System.Clock as Clock
import System.IO (hPutStrLn, stderr)
import System.Random (randomRIO)

import RecentList (RecentList)
import qualified RecentList as RL


-- CONSTANTS

maxNumActions :: Int
maxNumActions = 5

recordSecs :: Int64
recordSecs = 20

cleanupIntervalSecs :: Int
cleanupIntervalSecs = 3600

cleanupIntervalFuzzSecs :: Int
cleanupIntervalFuzzSecs = 120


-- IMPLEMENTATION

getTimeSecsMonotonic :: IO Int64
getTimeSecsMonotonic = Clock.sec <$> Clock.getTime Clock.Monotonic

newtype SpamDetect a = SpamDetect (TVar (Map a (RecentList Int64)))

initSpamDetect :: (Show a, Ord a) => IO (SpamDetect a)
initSpamDetect = do
    var <- newTVarIO mempty

    void $ forkIO $ forever $ do
        randoffset <- randomRIO (0, cleanupIntervalFuzzSecs)
        threadDelay (cleanupIntervalSecs * 1000 * 1000 + randoffset)
        cleanupMap var

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

cleanupMap :: (Show a, Ord a) => TVar (Map a (RecentList Int64)) -> IO ()
cleanupMap var = do
    now <- getTimeSecsMonotonic
    pairs <- Map.assocs <$> atomically (readTVar var)
    toDelete <- fmap catMaybes . forM pairs $ \(key, rl) -> do
        lastTime <- atomically $ RL.get rl (RL.size rl - 1)
        if now - lastTime > recordSecs
            then return (Just key)
            else return Nothing
    when (length toDelete > 0) $
        hPutStrLn stderr $ "Spam map cleanup: deleting: " ++ show toDelete
    let deleteMap = Map.fromList (map (,()) toDelete)
    -- There is a race condition here; technically, the entire function needs
    -- to be in an 'atomically' block, and the fact that it isn't means that
    -- events that happen halfway through may be lost. However, this can only
    -- result in false-negatives (stuff not marked spam even though it is
    -- spam), which is not all that bad.
    -- TODO: if execution of this function takes _really_ long, this can become
    -- a problem, but for now I'm assuming the RTS is nice to me.
    atomically $ modifyTVar var (Map.\\ deleteMap)
