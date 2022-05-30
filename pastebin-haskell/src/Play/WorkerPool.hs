{-# LANGUAGE LambdaCase #-}
{-| Intended to be imported qualified, e.g. as "WP". -}
module Play.WorkerPool (
  WPool,
  newPool,
  getAvailableVersions,
  submitJob,
  addWorker,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Client.TLS as N
import System.Clock (TimeSpec(..))
import qualified System.Clock as Clock
import System.IO (hPutStrLn, stderr)
import System.Random
import System.Timeout (timeout)

import Data.Queue (Queue)
import Data.Queue.Priority (PQueue)
import qualified Data.Queue.Priority as PQ
import qualified Data.Queue as Queue
import PlayHaskellTypes
import PlayHaskellTypes.Sign (PublicKey)
import qualified Play.WorkerPool.WorkerReqs as Worker


-- Here be dragons. There are a number of "global" variables (i.e. members of
-- 'WPool') that should be modified in the right places in order to keep the
-- statistics consistent.
--
-- When a new 'WPool' is created (with 'newPool'), a thread is spawned
-- ('poolHandlerLoop') that waits on events in the 'wpEventQueue'. If the queue
-- is currently empty, it waits on 'wpWakeup' to signal that there is a new
-- event in the queue. Hence, every time an event is pushed to the queue,
-- 'wpWakeup' needs to be signalled (by storing a unit in it). The wakeup
-- should be signalled /after/ the push to the queue to ensure that the handler
-- loop doesn't miss anything.
--
-- There are two remaining fields in 'WPool': 'wpVersions' and
-- 'wpNumQueuedJobs'.
-- - 'wpVersions' records the GHC versions available in any workers. It is
--   updated by the handlers for 'EVersionRefresh' in the event handler loop,
--   and should not be modified (only read using 'getAvailableVersions') from
--   the outside.
-- - 'wpNumQueuedJobs' records the current number of jobs submitted but not yet
--   being processed because all workers are already busy. The idea is that if
--   this number is too large, new jobs should probably not be accepted
--   anymore.
--   The field is /incremented/ whenever an 'ENewJob' event is pushed to the
--   event queue, and /decremented/ whenever a job is submitted to a worker. In
--   the mean time, the job may spend some time in 'psBacklog' if no worker is
--   available immediately.
--
-- The 'PoolState' is the local state of the event handler loop, and contains:
-- - A map of all workers indexed by hostname to ensure there is only one per
--   hostname ('psWorkers'),
-- - Sets indicating the idle ('psIdle'), busy ('psBusy') and disabled
--   ('psDisabled') workers, being disjoint and forming together the full set
--   of workers.
-- - The backlog of jobs whose 'ENewJob' event was already processed, but for
--   which there is no worker available yet. These jobs still count towards
--   'wpNumQueuedJobs'.
-- - A random number generator.
--
-- TODO: describe 'WStatus' and check that the 'wStatus' is always in sync with
-- the 'psIdle', 'psBusy', 'psDisabled' sets.


data Job = Job RunRequest (RunResponse -> IO ())

data Event = EAddWorker ByteString PublicKey
           | ENewJob Job
           | EWorkerIdle Worker.Addr
           | EVersionRefresh Worker.Addr

data WPool = WPool
  { wpVersions :: TVar [Version]  -- ^ Currently available versions
  , wpNumQueuedJobs :: TVar Int  -- ^ Number of jobs that have been submitted but not yet sent to a worker
  , wpEventQueue :: TVar (PQueue TimeSpec Event)  -- ^ Event queue
  , wpWakeup :: TMVar ()  -- ^ Wakeup channel
  , wpMaxQueuedJobs :: Int
  }

data PoolState = PoolState
  { psWorkers :: Map ByteString Worker  -- ^ hostname -> worker
  , psIdle :: Set Worker.Addr
  , psBusy :: Set Worker.Addr
  , psDisabled :: Set Worker.Addr
  , psBacklog :: Queue Job
  , psRNG :: StdGen
  }

data WStatus = OK
             | Disabled TimeSpec  -- ^ Last liveness check ('Monotonic' clock)
                        TimeSpec  -- ^ Current wait interval
  deriving (Show)

data Worker = Worker
  { wAddr :: Worker.Addr
  , wStatus :: WStatus
  , wVersions :: [Version]
  }

newPool :: Int -> IO WPool
newPool maxqueuedjobs = do
  mgr <- N.newTlsManager
  vervar <- newTVarIO []
  numqueuedvar <- newTVarIO 0
  queuevar <- newTVarIO PQ.empty
  wakeupvar <- newEmptyTMVarIO
  rng <- newStdGen
  let wpool = WPool { wpVersions = vervar
                    , wpNumQueuedJobs = numqueuedvar
                    , wpEventQueue = queuevar
                    , wpWakeup = wakeupvar
                    , wpMaxQueuedJobs = maxqueuedjobs }
      state = PoolState { psWorkers = mempty
                        , psIdle = mempty
                        , psBusy = mempty
                        , psDisabled = mempty
                        , psBacklog = Queue.empty
                        , psRNG = rng }
  _ <- forkIO $ poolHandlerLoop wpool state mgr
  return wpool

poolHandlerLoop :: WPool -> PoolState -> N.Manager -> IO ()
poolHandlerLoop wpool initState mgr =
  let loop s = singleIteration s >>= loop
  in loop initState
  where
    singleIteration :: PoolState -> IO PoolState
    singleIteration state = do
      now <- Clock.getTime Clock.Monotonic

      -- Right Event: should handle event now
      -- Left (Just TimeSpec): should sleep until time (or wakeup)
      -- Left Nothing: no events in queue, should sleep until wakeup
      result <- atomically $ do
        queue <- readTVar (wpEventQueue wpool)
        case PQ.pop queue of
          Just ((at, event), queue')
            | at <= now -> do
                writeTVar (wpEventQueue wpool) $! queue'
                return (Right event)
            | otherwise -> return (Left (Just at))
          Nothing -> return (Left Nothing)

      case result of
        Right event -> do
          handleEvent wpool state mgr event
        Left Nothing -> do
          atomically $ takeTMVar (wpWakeup wpool)
          return state
        Left (Just time) -> do
          -- Get the current time again because we did some STM operations, which
          -- can retry and take some time
          now' <- Clock.getTime Clock.Monotonic
          let diff_us = min (fromIntegral (maxBound :: Int))
                            (Clock.toNanoSecs (time - now') `div` 1000)
          -- We don't care whether the timeout expired or whether we got woken up;
          -- in any case, loop around.
          _ <- timeout (fromIntegral diff_us) $ atomically $ takeTMVar (wpWakeup wpool)
          return state

handleEvent :: WPool -> PoolState -> N.Manager -> Event -> IO PoolState
handleEvent wpool state mgr = \case
  EAddWorker host pkey -> do
    let addr = Worker.Addr host pkey
    case Map.lookup host (psWorkers state) of
      Just{} -> do
        hPutStrLn stderr $ "A worker with this host already in pool: " ++ show addr
        atomically $ submitEvent wpool 0 (EVersionRefresh addr)
        return state
      Nothing -> do
        atomically $ submitEvent wpool 0 (EVersionRefresh addr)
        now <- Clock.getTime Clock.Monotonic
        let worker = Worker { wAddr = addr
                            , wStatus = Disabled now 0
                            , wVersions = [] }
        return state { psWorkers = Map.insert host worker (psWorkers state)
                     , psDisabled = Set.insert addr (psDisabled state) }

  ENewJob job
    | Set.null (psIdle state) ->
        -- Don't need to increment wpNumQueuedJobs because the job already got
        -- added to that counter when it was submitted to the event queue.
        return state { psBacklog = Queue.push (psBacklog state) job }
    | otherwise -> do
        let (idx, rng') = uniformR (0, Set.size (psIdle state) - 1) (psRNG state)
            addr@(Worker.Addr host _) = Set.elemAt idx (psIdle state)
            idle' = Set.deleteAt idx (psIdle state)
        -- TODO: What 'f wpool worker job mgr' should do is:
        -- - Send the request from 'job' to the worker over https using 'mgr'
        -- - Wait for the response to come in
        -- - Check the signature and pkey of the response with 'worker'
        -- - Call the callback in 'job' with the response
        -- - Call 'submitEvent wpool 0 (EWorkerIdle (wAddr worker))'
        todoSubmitJobToWorker wpool (psWorkers state Map.! host) job mgr
        return state { psIdle = idle'
                     , psBusy = Set.insert addr (psBusy state)
                     , psRNG = rng' }

  EWorkerIdle addr@(Worker.Addr host _)
    | Just (job, backlog') <- Queue.pop (psBacklog state) -> do
        -- Yay, we've unqueued a job, so we can decrement the counter
        atomically $ modifyTVar' (wpNumQueuedJobs wpool) pred
        todoSubmitJobToWorker wpool (psWorkers state Map.! host) job mgr
        return state { psBusy = Set.insert addr (psBusy state)
                     , psBacklog = backlog'
                     , psIdle = Set.delete addr (psIdle state)
                     , psDisabled = Set.delete addr (psDisabled state) }
    | otherwise ->
        return state { psIdle = Set.insert addr (psIdle state)
                     , psBusy = Set.delete addr (psBusy state)
                     , psDisabled = Set.delete addr (psDisabled state) }

  -- TODO: if the previous status was Disabled and the version succeeds, we
  -- should ensure that this produces EWorkerIdle so that it can pick up jobs
  -- from the backlog.
  -- Also make sure to think about exponential backoff.
  -- Also: update the vervar in the wpool!
  -- If you don't do anything with wStatus here, remove that field because it's unused otherwise.
  EVersionRefresh addr -> _

submitEvent :: WPool -> TimeSpec -> Event -> STM ()
submitEvent wpool at event = do
  modifyTVar' (wpEventQueue wpool) $ PQ.insert at event
  -- If there was already a wakeup signal there, don't do anything
  _ <- tryPutTMVar (wpWakeup wpool) ()
  return ()

getAvailableVersions :: WPool -> IO [Version]
getAvailableVersions wpool = readTVarIO (wpVersions wpool)

-- | If this returns 'Nothing', the backlog was full and the client should try
-- again later.
submitJob :: WPool -> RunRequest -> IO (Maybe RunResponse)
submitJob wpool req = do
  chan <- newTChanIO
  submitted <- atomically $ do
    numqueued <- readTVar (wpNumQueuedJobs wpool)
    if numqueued >= wpMaxQueuedJobs wpool
      then return False
      else do submitEvent wpool 0 (ENewJob (Job req (atomically . writeTChan chan)))
              return True
  if submitted
    then Just <$> atomically (readTChan chan)
    else return Nothing

addWorker :: WPool -> ByteString -> PublicKey -> IO ()
addWorker wpool host publickey = do
  atomically $ modifyTVar' (wpEventQueue wpool) $
    PQ.insert 0 (EAddWorker host publickey)
  -- let addr = Worker.Addr host
  -- now <- Clock.getTime Clock.Monotonic
  -- localQueue <- newTVarIO Queue.empty
  -- atomically $ modifyTVar' wpvar $ \wp -> do
  --   let worker = Worker { wHostname = addr
  --                       , wPKey = publickey
  --                       , wStatus = Disabled now 0
  --                       , wVersions = []
  --                       , wLocalQueue = localQueue
  --                       }
  --   -- Don't put it in wpIdle yet; a successful version refresh will do that.
  --   wp { wpWorkers = Map.insert addr worker (wpWorkers wp) }
  -- forkRefreshVersions wpool addr

-- forkRefreshVersions :: WPool -> Worker.Addr -> IO ()
-- forkRefreshVersions wpool@(WPool mgr wpvar) addr = do
--   void $ forkIO $ do
--     waitClaimWorker wpool addr
--     wp <- readTVarIO wpvar
--     case Map.lookup addr (wpWorkers wp) of
--       Nothing -> return ()  -- Nothing to do, not sure what went wrong
--       Just worker ->
--         Worker.getVersions mgr addr >>= \case
--           Just versions ->
--             modifyWorker wpool addr (wPKey worker) $ \w ->
--               w { wStatus = OK, wVersions = versions }
--           Nothing -> do
--             now <- Clock.getTime Clock.Monotonic
--             let iv = case wStatus worker of
--             modifyWorker wpool addr $ \w ->
--               w { wStatus = Disabled now healthCheckIvStart, wVersions = [] }

-- waitClaimWorker :: WPool -> Worker.Addr -> IO ()
-- waitClaimWorker (WPool _ wpvar) addr = do
--   atomically $ do
--     wp <- readTVar wpvar
--     worker <- case Map.lookup addr (wpWorkers wp) of
--       Just worker -> return worker
--       Nothing -> error $ "waitClaimWorker: Worker not found: " ++ show addr
--     _

-- scheduleWhenIdle :: WPool -> Worker.Addr -> (Worker -> IO Worker) -> IO ()
-- scheduleWhenIdle wpool@(WPool _ wpvar) addr f = do
--   lockedNow <- atomically $ do
--     wp <- readTVar wpvar
--     let (newWorker, lockedNow) =
--           case Map.lookup addr (wpWorkers wp) of
--             Just w@(Worker { wBusy = False }) ->
--               (w { wBusy = True }, True)
--             Just w@(Worker { wBusy = True }) ->
--               let action = scheduleWhenIdle wpool addr f
--               in (w { wIdleQueue = Queue.push (wIdleQueue w) action }, False)
--             Nothing -> error $ "Worker not found in map: " ++ show addr
--     writeTVar wpvar $!
--       wp { wpWorkers = Map.insert addr newWorker (wpWorkers wp) }
--     return lockedNow

--   when lockedNow $ do
--     wp <- atomically $ readTVar wpvar
--     case Map.lookup addr (wpWorkers wp) of
--       Just worker -> do
--         worker' <- f worker
--         let worker'' = worker' { wBusy = False }
--         atomically $ writeTVar wpvar $!
--           wp { wpWorkers = Map.insert addr worker'' (wpWorkers wp) }
--       Nothing -> error $ "Worker not found in map: " ++ show addr
--     return ()

-- modifyWorker :: WPool -> Worker.Addr -> (Worker -> Worker) -> IO ()
-- modifyWorker (WPool _ wpvar) addr f =
--   atomically $ modifyTVar' wpvar $ \wp ->
--     wp { wpWorkers = case Map.lookup addr (wpWorkers wp) of
--                        Just w -> Map.insert addr (f w) (wpWorkers wp)
--                        Nothing -> error $ "Worker not found in map: " ++ show addr }

healthCheckIvStart :: TimeSpec
healthCheckIvStart = TimeSpec 1 0

-- 1.5 * previous, but start with something positive (1.5s) if previous was tiny
healthCheckIvNext :: TimeSpec -> TimeSpec
healthCheckIvNext ts =
  let prev = max ts (TimeSpec 1 0)
  in min (TimeSpec 3600 0) (3 * prev `div` 2)
