module Data.Queue (
  Queue,
  empty, push, pop, size
) where


data Queue a = Queue Int  -- size
                     [a]  -- popping end
                     [a]  -- pushing end
  deriving (Show)

empty :: Queue a
empty = Queue 0 [] []

push :: Queue a -> a -> Queue a
push (Queue sz front back) x = Queue (sz + 1) front (x : back)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue sz (x : front) back) = Just (x, Queue (sz - 1) front back)
pop (Queue _ [] []) = Nothing
pop (Queue sz [] back) = pop (Queue sz (reverse back) [])

size :: Queue a -> Int
size (Queue sz _ _) = sz
