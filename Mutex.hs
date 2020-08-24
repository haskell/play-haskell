module Mutex where

import Control.Concurrent.MVar
import Control.Exception (bracket_)


newtype Mutex = Mutex (MVar ())

newMutex :: IO Mutex
newMutex = Mutex <$> newMVar ()

withLock :: Mutex -> IO () -> IO ()
withLock (Mutex mvar) = bracket_ (takeMVar mvar) (putMVar mvar ())
