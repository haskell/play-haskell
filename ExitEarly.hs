module ExitEarly (
  ExitEarlyT,
  exitEarly,
  execExitEarlyT,
  okOrExitEarly,
  lift,
) where

import Control.Monad (ap)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class


newtype ExitEarlyT r m a = ExitEarlyT { runExitEarlyT :: (a -> m r) -> m r }

instance Functor m => Functor (ExitEarlyT r m) where
  fmap f (ExitEarlyT g) = ExitEarlyT (\k -> g (k . f))

instance Monad m => Applicative (ExitEarlyT r m) where
  pure x = ExitEarlyT ($ x)
  (<*>) = ap

instance Monad m => Monad (ExitEarlyT r m) where
  ExitEarlyT g >>= f = ExitEarlyT (\k -> g (($ k) . runExitEarlyT . f))

instance MonadTrans (ExitEarlyT r) where
  lift act = ExitEarlyT (act >>=)

instance MonadIO m => MonadIO (ExitEarlyT r m) where
  liftIO act = ExitEarlyT (liftIO act >>=)

exitEarly :: Monad m => r -> ExitEarlyT r m a
exitEarly r = ExitEarlyT (\_ -> return r)

okOrExitEarly :: Monad m => Either e a -> (e -> ExitEarlyT r m r) -> ExitEarlyT r m a
okOrExitEarly (Left err) f = f err >>= exitEarly
okOrExitEarly (Right x) _ = return x

execExitEarlyT :: Monad m => ExitEarlyT r m r -> m r
execExitEarlyT (ExitEarlyT g) = g return
