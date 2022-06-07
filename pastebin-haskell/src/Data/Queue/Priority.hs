module Data.Queue.Priority (
  PQueue,
  empty,
  insert,
  pop,
  Data.Queue.Priority.length,
) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


newtype PQueue p a = PQueue (Map p (NonEmpty a))

-- | O(1).
empty :: PQueue p a
empty = PQueue Map.empty

-- | O(log(n)).
insert :: Ord p => p -> a -> PQueue p a -> PQueue p a
insert prio x (PQueue m) = PQueue (Map.insertWith (<>) prio (x :| []) m)

-- | O(log(n)).
pop :: Ord p => PQueue p a -> Maybe ((p, a), PQueue p a)
pop (PQueue m) =
  case Map.minViewWithKey m of
    Nothing -> Nothing
    Just ((p, x :| []), m') -> Just ((p, x), PQueue m')
    Just ((p, x :| (y:ys)), m') -> Just ((p, x), PQueue (Map.insert p (y :| ys) m'))

-- | Note: O(n).
length :: PQueue p a -> Int
length (PQueue m) = sum (map NE.length (Map.elems m))
