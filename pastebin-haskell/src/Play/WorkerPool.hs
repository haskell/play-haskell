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
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.List (sort)
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
import PlayHaskellTypes.Sign (PublicKey, SecretKey)
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
-- should be signalled /after/ (or simultaneously with) the push to the queue
-- to ensure that the handler loop doesn't miss anything.
--
-- The only place where an event is pushed to the queue is in 'submitEvent', so
-- it is there that we need to signal 'wpWakeup' as well -- and we do.
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
--   anymore. (This is handled in the check against 'wpMaxQueuedJobs' in
--   'submitJob'.)
--   The field is /incremented/ whenever an 'ENewJob' event is pushed to the
--   event queue, and /decremented/ whenever a job is submitted to a worker. In
--   the mean time, the job may spend some time in 'psBacklog' if no worker is
--   available immediately.
--
-- The 'PoolState' is the local state of the event handler loop, and contains:
-- - A map of all workers indexed by hostname to ensure there is only one per
--   hostname ('psWorkers').
-- - A set indicating the idle workers ('psIdle').
-- - The backlog of jobs whose 'ENewJob' event was already processed, but for
--   which there is no worker available yet. These jobs still count towards
--   'wpNumQueuedJobs'.
-- - A random number generator.
--
-- A worker is described by the 'Worker' record, containing its address
-- (hostname and public key), status, and list of offered GHC versions. If a
-- worker is disabled, we furthermore store when we last checked on the worker,
-- and how long we're waiting to re-check since that time (in an exponential
-- backoff scheme).
--
-- Invariant: if a worker has the Disabled status, then one of the
-- EVersionRefresh, EWorkerFailed, or EWorkerVersions events is scheduled for
-- that worker.
--
-- "Checking upon a worker" means sending it a version listing request, and the
-- event handler that can un-disable a worker is, hence, the one for
-- 'EVersionRefresh'.


-- | The response handler is called in a forkIO thread.
data Job = Job RunRequest (RunResponse -> IO ())

data Event = EAddWorker ByteString PublicKey  -- ^ New worker
           | ENewJob Job  -- ^ New job has arrived!
           | EWorkerIdle Worker.Addr  -- ^ Worker has become idle
           | EVersionRefresh Worker.Addr  -- ^ Should refresh versions now
           | EWorkerFailed Worker.Addr  -- ^ Should be marked disabled
           | EWorkerVersions Worker.Addr [Version]  -- ^ Version check succeeded

data WPool = WPool
  { wpVersions :: TVar [Version]  -- ^ Currently available versions
  , wpNumQueuedJobs :: TVar Int  -- ^ Number of jobs that have been submitted but not yet sent to a worker
  , wpEventQueue :: TVar (PQueue TimeSpec Event)  -- ^ Event queue
  , wpWakeup :: TMVar ()  -- ^ Wakeup channel
  , wpMaxQueuedJobs :: Int
  , wpSecretKey :: SecretKey
  }

data PoolState = PoolState
  { psWorkers :: Map ByteString Worker  -- ^ hostname -> worker
  , psIdle :: Set Worker.Addr
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

newPool :: SecretKey -> Int -> IO WPool
newPool serverSkey maxqueuedjobs = do
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
                    , wpMaxQueuedJobs = maxqueuedjobs
                    , wpSecretKey = serverSkey }
      state = PoolState { psWorkers = mempty
                        , psIdle = mempty
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
    if host `Map.member` psWorkers state
      then do
        hPutStrLn stderr $ "A worker with this host already in pool: " ++ show addr
        atomically $ submitEvent wpool 0 (EVersionRefresh addr)
        return state
      else do
        atomically $ submitEvent wpool 0 (EVersionRefresh addr)
        now <- Clock.getTime Clock.Monotonic
        let worker = Worker { wAddr = addr
                            , wStatus = Disabled now 0
                            , wVersions = [] }
        return state { psWorkers = Map.insert host worker (psWorkers state) }

  ENewJob job
    | Map.null (psWorkers state) -> do
        -- If there are no workers at all, don't accept the job
        atomically $ modifyTVar' (wpNumQueuedJobs wpool) pred
        let Job _ callback = job
        _ <- forkIO $ callback (RunResponseErr REBackend)
        return state
    | Set.null (psIdle state) ->
        -- Don't need to increment wpNumQueuedJobs because the job already got
        -- added to that counter when it was submitted to the event queue.
        return state { psBacklog = Queue.push (psBacklog state) job }
    | otherwise -> do
        -- select a random worker
        let (idx, rng') = uniformR (0, Set.size (psIdle state) - 1) (psRNG state)
            Worker.Addr host _ = Set.elemAt idx (psIdle state)
            idle' = Set.deleteAt idx (psIdle state)
        -- Yay, we've unqueued a job, so we can decrement the counter
        atomically $ modifyTVar' (wpNumQueuedJobs wpool) pred
        sendJobToWorker wpool (psWorkers state Map.! host) job mgr
        return state { psIdle = idle'
                     , psRNG = rng' }

  EWorkerIdle addr@(Worker.Addr host _)
    | Just Worker{wStatus=Disabled{}} <- Map.lookup host (psWorkers state) -> do
        -- Since we are already health-checking the worker (that process was
        -- started when the worker entered Disabled state), we don't need to
        -- start health-checking it here. Just ensure it's not marked idle.
        return state { psIdle = Set.delete addr (psIdle state) }

    | Just (job, backlog') <- Queue.pop (psBacklog state) -> do
        -- Yay, we've unqueued a job, so we can decrement the counter
        atomically $ modifyTVar' (wpNumQueuedJobs wpool) pred
        sendJobToWorker wpool (psWorkers state Map.! host) job mgr
        -- We don't know whether it was idle before, but for sure it isn't now.
        return state { psIdle = Set.delete addr (psIdle state)
                     , psBacklog = backlog' }

    | otherwise ->
        -- No queued job to give to this worker, so just mark it as idle.
        return state { psIdle = Set.insert addr (psIdle state) }

  EVersionRefresh addr -> do
    _ <- forkIO $ do
      Worker.getVersions mgr addr >>= \case
        Just vers -> atomically $ submitEvent wpool 0 (EWorkerVersions addr vers)
        Nothing -> atomically $ submitEvent wpool 0 (EWorkerFailed addr)

    return state

  EWorkerFailed addr@(Worker.Addr host _)
    | Just worker <- Map.lookup host (psWorkers state) -> do
        now <- Clock.getTime Clock.Monotonic
        let iv = case wStatus worker of
                   OK -> healthCheckIvStart
                   Disabled _ iv' -> healthCheckIvNext iv'
            worker' = worker { wStatus = Disabled now iv }
        return state { psWorkers = Map.insert host worker' (psWorkers state) }

    | otherwise -> do
        hPutStrLn stderr $ "[EWF] Worker does not exist: " ++ show addr
        return state

  -- TODO: if the previous status was Disabled, we should ensure that this
  -- produces EWorkerIdle so that it can pick up jobs from the backlog.
  -- Also: update the vervar in the wpool!
  -- If you don't do anything with wStatus here, remove that field because it's unused otherwise.
  EWorkerVersions addr@(Worker.Addr host _) versions
    | Just worker <- Map.lookup host (psWorkers state) -> do
        -- If the worker was disabled before, notify that it's idle now
        case wStatus worker of
          OK -> return ()
          Disabled{} -> atomically $ submitEvent wpool 0 (EWorkerIdle addr)

        -- Update the available versions in the WPool
        atomically $ do
          allvers <- readTVar (wpVersions wpool)
          let uniq (x:y:xs) | x == y = uniq (y:xs)
                            | otherwise = x : uniq (y:xs)
              uniq l = l
          writeTVar (wpVersions wpool) (uniq (sort (allvers ++ versions)))

        -- Note that we don't put the worker in the psIdle set here yet; that's
        -- the task of the EWorkerIdle handler.
        let worker' = worker { wStatus = OK
                             , wVersions = versions }
        return state { psWorkers = Map.insert host worker' (psWorkers state) }

    | otherwise -> do
        hPutStrLn stderr $ "[EWV] Worker does not exist: " ++ show addr
        return state

sendJobToWorker :: WPool -> Worker -> Job -> N.Manager -> IO ()
sendJobToWorker wpool worker (Job runreq resphandler) mgr =
  void $ forkIO $ do
    result <- Worker.runJob (wpSecretKey wpool) mgr (wAddr worker) runreq
    case result of
      Just response -> do
        _ <- forkIO $ resphandler response
        atomically $ submitEvent wpool 0 (EWorkerIdle (wAddr worker))
      Nothing -> do
        _ <- forkIO $ resphandler (RunResponseErr REBackend)
        atomically $ submitEvent wpool 0 (EWorkerFailed (wAddr worker))

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
addWorker wpool host publickey =
  atomically $ submitEvent wpool 0 (EAddWorker host publickey)
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
