{-| A thread-safe cache of the last N items in a stream. -}
module RecentList where

import Control.Concurrent.STM
import Data.Array (Array)
import qualified Data.Array as A


-- RecentList (cursor) (items)
-- The last N items, in chronological order, are at cursor-N+1, cursor-N+2, ..., cursor-1 (all mod length).
data RecentList a = RecentList (TVar Int) (Array Int (TVar a))

new :: [a] -> STM (RecentList a)
new l = RecentList <$> newTVar (length l - 1)
                   <*> (A.listArray (0, length l - 1) <$> traverse newTVar l)

size :: RecentList a -> Int
size (RecentList _ arrv) = snd (A.bounds arrv) + 1

add :: RecentList a -> a -> STM ()
add (RecentList vcursor arrv) value = do
    cursor <- readTVar vcursor
    writeTVar (arrv A.! cursor) value
    let len = snd (A.bounds arrv) + 1
    writeTVar vcursor ((cursor + 1) `rem` len)

-- 0 -> oldest; N-1 -> newest
get :: RecentList a -> Int -> STM a
get (RecentList vcursor arrv) idx
  | 0 <= idx, idx <= snd (A.bounds arrv) = do
      let len = snd (A.bounds arrv) + 1
      cursor <- readTVar vcursor
      readTVar (arrv A.! ((cursor + idx) `rem` len))
  | otherwise = error "RecentList.get: index out of bounds"
