module Play.WorkerPool where

import Control.Concurrent.STM

import PlayHaskellTypes
import qualified Play.WorkerPool.WorkerReqs as Worker


data WPool = WPool (TVar WP)
data WP = WP
  { wpWorkers :: [Worker] }
  deriving (Show)

data Worker = Worker
  { wHostname :: Worker.Addr
  , wVersions :: [Version] }
  deriving (Show)


