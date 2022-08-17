{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-| Intended to be imported qualified, e.g. as \"WP". -}
module Play.WorkerPool (
  WPool,
  newPool,
  -- * Getting info
  getAvailableVersions,
  Status(..),
  WorkerStatus(..),
  getPoolStatus,
  -- * Putting stuff
  submitJob,
  addWorker,
  removeWorker,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (void)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Client.TLS as N
import Text.Show.Functions ()
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
  deriving (Show)

data Status = Status
  { statWorkers :: [WorkerStatus]
  , statJobQueueLength :: Int
  , statEventQueueLength :: Int
  , statNow :: TimeSpec  -- ^ Current time to compare Disabled times with
  }
  deriving (Show, Generic)

data WorkerStatus = WorkerStatus
  { wstatAddr :: Worker.Addr
  , wstatDisabled :: Maybe (TimeSpec, TimeSpec)  -- ^ (last check, wait interval)
  , wstatVersions :: [Version]
  , wstatIdle :: Bool }
  deriving (Show)

newtype TimeSpecJSON = TimeSpecJSON TimeSpec
  deriving (Show)

instance J.ToJSON TimeSpecJSON where
  toJSON (TimeSpecJSON (TimeSpec s ns)) = J.object
    [fromString "sec" J..= s, fromString "nsec" J..= ns]
  toEncoding (TimeSpecJSON (TimeSpec s ns)) = JE.pairs $
    fromString "sec" J..= s <> fromString "nsec" J..= ns

instance J.ToJSON Status where
  toJSON (Status workers jql eql now) =
    J.object [fromString "workers" J..= workers
             ,fromString "job_queue_length" J..= jql
             ,fromString "event_queue_length" J..= eql
             ,fromString "now" J..= TimeSpecJSON now]
  toEncoding (Status workers jql eql now) =
    JE.pairs (fromString "workers" J..= workers
           <> fromString "job_queue_length" J..= jql
           <> fromString "event_queue_length" J..= eql
           <> fromString "now" J..= TimeSpecJSON now)

instance J.ToJSON WorkerStatus where
  toJSON (WorkerStatus (Worker.Addr host pkey) disabled versions idle) =
    J.object [fromString "addr" J..= (Char8.unpack host, pkey)
             ,fromString "disabled" J..=
                case disabled of
                  Nothing -> J.Null
                  Just (tm, iv) -> J.toJSON (TimeSpecJSON tm, TimeSpecJSON iv)
             ,fromString "versions" J..= versions
             ,fromString "idle" J..= idle]
  toEncoding (WorkerStatus (Worker.Addr host pkey) disabled versions idle) =
    JE.pairs (fromString "addr" J..= (Char8.unpack host, pkey)
           <> fromString "disabled" J..=
                case disabled of
                  Nothing -> J.Null
                  Just (tm, iv) -> J.toJSON (TimeSpecJSON tm, TimeSpecJSON iv)
           <> fromString "versions" J..= versions
           <> fromString "idle" J..= idle)

data Event = EAddWorker ByteString PublicKey  -- ^ New worker
           | ERemoveWorker ByteString  -- ^ Remove worker from pool
           | ENewJob Job  -- ^ New job has arrived!
           | EWorkerIdle Worker.Addr  -- ^ Worker has become idle
           | EVersionRefresh Worker.Addr  -- ^ Should refresh versions now
           | EWorkerFailed Worker.Addr  -- ^ Should be marked disabled
           | EWorkerVersions Worker.Addr [Version]  -- ^ Version check succeeded
           | EStatus (Status -> IO ())  -- ^ Called in forkIO
  deriving (Show)

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
  , psToBeRemoved :: Set ByteString
    -- ^ Hostnames for which a remove request came in while worker was busy,
    -- remove when their job finishes
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

-- | Create a new worker pool. This function needs the secret key of the
-- server, and furthermore takes the maximum number /n/ of queued jobs (i.e.
-- jobs that need to be sent to a worker, but all workers are currently busy).
-- If there are /w/ workers, then a maximum of /w/ + /n/ jobs can be in flight
-- at any one time; if more jobs are added, 'submitJob' will return 'Nothing'.
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
                        , psToBeRemoved = mempty
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
handleEvent wpool state mgr event = do
  hPutStrLn stderr $ "Handling event: " ++ show event
  handleEvent' wpool state mgr event

handleEvent' :: WPool -> PoolState -> N.Manager -> Event -> IO PoolState
handleEvent' wpool state mgr = \case
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
        let state' = state { psWorkers = Map.insert host worker (psWorkers state) }
        recomputeAvailableVersions wpool state'
        return state'

  ERemoveWorker host ->
    case Map.lookup host (psWorkers state) of
      Just Worker{wStatus = Disabled{}} ->
        -- No need to recompute available versions, because this worker was
        -- disabled anyway (and disabled workers don't influence the set of
        -- available versions)
        return state { psWorkers = Map.delete host (psWorkers state) }

      Just Worker{wAddr = addr, wStatus = OK} -> do
        let state'
              | addr `Set.member` psIdle state =
                  state { psWorkers = Map.delete host (psWorkers state)
                        , psIdle = Set.delete addr (psIdle state) }
              | otherwise =
                  state { psToBeRemoved = Set.insert host (psToBeRemoved state) }
        recomputeAvailableVersions wpool state'
        return state'

      Nothing -> return state  -- worker didn't even exist

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
    | host `Set.member` psToBeRemoved state ->
        return state { psWorkers = Map.delete host (psWorkers state)
                     , psIdle = Set.delete addr (psIdle state)
                     , psToBeRemoved = Set.delete host (psToBeRemoved state) }

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
    | host `Set.member` psToBeRemoved state ->
        return state { psWorkers = Map.delete host (psWorkers state)
                     , psIdle = Set.delete addr (psIdle state)
                     , psToBeRemoved = Set.delete host (psToBeRemoved state) }

    | Just worker <- Map.lookup host (psWorkers state) -> do
        now <- Clock.getTime Clock.Monotonic
        let iv = case wStatus worker of
                   OK -> healthCheckIvStart
                   Disabled _ iv' -> healthCheckIvNext iv'
            worker' = worker { wStatus = Disabled now iv }
        atomically $ submitEvent wpool (now + iv) (EVersionRefresh addr)
        let state' = state { psWorkers = Map.insert host worker' (psWorkers state) }
        recomputeAvailableVersions wpool state'
        return state'

    | otherwise -> do
        hPutStrLn stderr $ "[EWF] Worker does not exist: " ++ show addr
        return state

  EWorkerVersions addr@(Worker.Addr host _) versions
    | Just worker <- Map.lookup host (psWorkers state) -> do
        -- If the worker was disabled before, notify that it's idle now
        case wStatus worker of
          OK -> return ()
          Disabled{} -> atomically $ submitEvent wpool 0 (EWorkerIdle addr)

        -- Note that we don't put the worker in the psIdle set here yet; that's
        -- the task of the EWorkerIdle handler.
        let worker' = worker { wStatus = OK
                             , wVersions = versions }
        let state' = state { psWorkers = Map.insert host worker' (psWorkers state) }
        recomputeAvailableVersions wpool state'
        return state'

    | otherwise -> do
        hPutStrLn stderr $ "[EWV] Worker does not exist: " ++ show addr
        return state

  EStatus callback -> do
    status <- collectStatus wpool state
    _ <- forkIO $ callback status
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

-- Intersect the version sets of all workers that are not disabled and not
-- to-be-removed, and store the result in wpVersions.
recomputeAvailableVersions :: WPool -> PoolState -> IO ()
recomputeAvailableVersions wpool state = do
  let perworker = [Set.fromList (wVersions worker)
                  | (host, worker) <- Map.assocs (psWorkers state)
                  , OK <- [wStatus worker]
                  , host `Set.notMember` psToBeRemoved state]
      available = case perworker of
                    [] -> []
                    _ -> Set.toList $ foldl1 Set.intersection perworker
  atomically $ writeTVar (wpVersions wpool) $! available

-- | Get the GHC versions currently available in the pool.
--
-- This returns the intersection of the advertised availability of all active
-- workers. This is because the worker pool is not yet able to distribute work
-- depending on the requested version.
getAvailableVersions :: WPool -> IO [Version]
getAvailableVersions wpool = readTVarIO (wpVersions wpool)

-- | This may block for a while if the event queue is very full.
getPoolStatus :: WPool -> IO Status
getPoolStatus wpool = do
  var <- newEmptyTMVarIO
  atomically $ submitEvent wpool 0 (EStatus (atomically . putTMVar var))
  atomically $ readTMVar var

-- | If this returns 'Nothing', the backlog was full and the client should try
-- again later.
submitJob :: WPool -> RunRequest -> IO (Maybe RunResponse)
submitJob wpool req = do
  chan <- newTChanIO
  submitted <- atomically $ do
    numqueued <- readTVar (wpNumQueuedJobs wpool)
    if numqueued >= wpMaxQueuedJobs wpool
      then return False
      else do modifyTVar' (wpNumQueuedJobs wpool) succ
              submitEvent wpool 0 (ENewJob (Job req (atomically . writeTChan chan)))
              return True
  if submitted
    then Just <$> atomically (readTChan chan)
    else return Nothing

-- | If a worker with that host is already in the pool, no action is taken.
-- (That is to say: different workers must have different hostnames, but may
-- have equal public keys.)
addWorker :: WPool -> ByteString -> PublicKey -> IO ()
addWorker wpool host publickey
  | all (\b -> 32 < b && b < 127) (BS.unpack host) =
      atomically $ submitEvent wpool 0 (EAddWorker host publickey)
  | otherwise = ioError $ userError "Non-printable byte in host in addWorker"

-- | Remove the worker with the given hostname from the pool. If there is no
-- such worker, no action is taken.
removeWorker :: WPool -> ByteString -> IO ()
removeWorker wpool host =
  atomically $ submitEvent wpool 0 (ERemoveWorker host)

collectStatus :: WPool -> PoolState -> IO Status
collectStatus wpool state = do
  jqlen <- readTVarIO (wpNumQueuedJobs wpool)
  eqlen <- PQ.length <$> readTVarIO (wpEventQueue wpool)
  now <- Clock.getTime Clock.Monotonic
  return Status { statWorkers = map (makeWorkerStatus (psIdle state))
                                    (Map.elems (psWorkers state))
                , statJobQueueLength = jqlen
                , statEventQueueLength = eqlen
                , statNow = now }
  where
    makeWorkerStatus idleset worker = WorkerStatus
      { wstatAddr = wAddr worker
      , wstatDisabled = case wStatus worker of
                          OK -> Nothing
                          Disabled lastCheck iv -> Just (lastCheck, iv)
      , wstatVersions = wVersions worker
      , wstatIdle = wAddr worker `Set.member` idleset }

healthCheckIvStart :: TimeSpec
healthCheckIvStart = TimeSpec 1 0

-- 1.5 * previous, but start with something positive (1.5s) if previous was tiny
healthCheckIvNext :: TimeSpec -> TimeSpec
healthCheckIvNext ts =
  let prev = max ts (TimeSpec 1 0)
  in min (TimeSpec 3600 0) (3 * prev `div` 2)
