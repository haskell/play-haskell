module Data.Queue.Priority (
  PQueue,
  empty,
  insert,
  pop,
) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


newtype PQueue p a = PQueue (Map p (NonEmpty a))

empty :: PQueue p a
empty = PQueue Map.empty

insert :: Ord p => p -> a -> PQueue p a -> PQueue p a
insert prio x (PQueue m) = PQueue (Map.insertWith (<>) prio (x :| []) m)

pop :: Ord p => PQueue p a -> Maybe ((p, a), PQueue p a)
pop (PQueue m) =
  case Map.minViewWithKey m of
    Nothing -> Nothing
    Just ((p, x :| []), m') -> Just ((p, x), PQueue m')
    Just ((p, x :| (y:ys)), m') -> Just ((p, x), PQueue (Map.insert p (y :| ys) m'))
