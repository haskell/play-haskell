{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module GHCPool (
  availableVersions,
  runTimeoutMicrosecs,
  Pool,
  makePool,
  Result(..),
  runInPool,
) where

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import qualified System.Clock as Clock
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStr, hGetContents, hClose)
import System.Posix.Directory (getWorkingDirectory)
import System.Posix.Signals (signalProcess, sigKILL)
import qualified System.Process as Pr
import qualified System.Process.Internals as PrI
import System.Timeout (timeout)

import Data.Queue (Queue)
import qualified Data.Queue as Queue
import PlayHaskellTypes (Command(..), Optimisation(..), Version(..), RunError(..))


runTimeoutMicrosecs :: Int
runTimeoutMicrosecs = 5_000_000

availableVersions :: IO [String]
availableVersions = do
  out <- Pr.readCreateProcess (Pr.proc "ghcup" ["--offline", "list", "-t", "ghc", "-c", "installed", "-r"]) []
  let ghc_versions = [ver | (words -> _ : ver : _) <- lines out]
  return ghc_versions

commandString :: Command -> String
commandString CRun = "run"
commandString CCore = "core"
commandString CAsm = "asm"

optimisationString :: Optimisation -> String
optimisationString O0 = "-O0"
optimisationString O1 = "-O1"
optimisationString O2 = "-O2"

data Result = Result
  { resExitCode :: ExitCode
  , resStdout :: String
  , resStderr :: String
  , resTimeTaken :: Double  -- ^ seconds
  }
  deriving (Show)

type RunResult = Either RunError Result

data Worker = Worker ThreadId
                     (MVar (Command, Optimisation, Version, String))  -- ^ input
                     (MVar RunResult)  -- ^ output

data PoolData = PoolData
  { pdAvailable :: [Worker]
  , pdQueue :: Queue (MVar Worker) }

data Pool = Pool { pDataVar :: MVar PoolData }

makeWorker :: IO Worker
makeWorker = do
  mvar <- newEmptyMVar
  resultvar <- newEmptyMVar
  thread <- forkIO $ do
    workdir <- getWorkingDirectory
    let spec = (Pr.proc (workdir </> "bwrap-files/start.sh") [])
                  { Pr.std_in = Pr.CreatePipe
                  , Pr.std_out = Pr.CreatePipe
                  , Pr.std_err = Pr.CreatePipe }
    Pr.withCreateProcess spec $ \(Just inh) (Just outh) (Just errh) proch -> do
      (cmd, opt, Version ver, source) <- readMVar mvar
      _ <- forkIO $ do
        hPutStr inh (commandString cmd ++ "\n" ++ optimisationString opt ++ "\n" ++ ver ++ "\n" ++ source)
        hClose inh
      stdoutmvar <- newEmptyMVar
      _ <- forkIO $ hGetContents outh >>= evaluate . forceString >>= putMVar stdoutmvar
      stderrmvar <- newEmptyMVar
      _ <- forkIO $ hGetContents errh >>= evaluate . forceString >>= putMVar stderrmvar
      (dur, mec) <- duration $ timeout runTimeoutMicrosecs $ Pr.waitForProcess proch
      case mec of
        Just ec -> do
          out <- readMVar stdoutmvar
          err <- readMVar stderrmvar
          putMVar resultvar (Right (Result ec out err dur))
        Nothing -> do
          -- Paranoid termination is technically unnecessary since bwrap seems
          -- to kill its child with SIGKILL if it is itself killed using
          -- SIGTERM, which is what Pr.terminateProcess sends. However, let's
          -- be safe.
          terminateParanoid proch
          putMVar resultvar (Left RETimeOut)
  return (Worker thread mvar resultvar)

-- | makePool numWorkers maxQueueLen
makePool :: Int -> IO Pool
makePool numWorkers = do
  -- putStrLn $ "Making a GHCPool with numWorkers=" ++ show numWorkers
  workers <- replicateM numWorkers makeWorker
  let pd = PoolData { pdAvailable = workers
                    , pdQueue = Queue.empty }
  pdvar <- newMVar pd
  return (Pool pdvar)

data ObtainedWorker = Obtained Worker
                    | Queued (MVar Worker) Int

runInPool :: Pool -> Command -> Version -> Optimisation -> String -> IO (Either RunError Result)
runInPool pool cmd ver opt source = do
  result <- modifyMVar (pDataVar pool) $ \pd ->
              case pdAvailable pd of
                w:ws ->
                  return (pd { pdAvailable = ws }, Obtained w)
                [] -> do
                  receptor <- newEmptyMVar
                  let newq = Queue.push (pdQueue pd) receptor
                  return (pd { pdQueue = newq }
                         ,Queued receptor (Queue.size newq))

  case result of
    Obtained worker -> do -- putStrLn "[obtained]"
                          useWorker worker
    Queued receptor _newlen -> do -- putStrLn ("[queued len=" ++ show newlen ++ "]")
                                  readMVar receptor >>= useWorker
  where
    useWorker :: Worker -> IO (Either RunError Result)
    useWorker (Worker _tid invar outvar) = do
      putMVar invar (cmd, opt, ver, source)
      result <- readMVar outvar
      _ <- forkIO $ do
        newWorker <- makeWorker
        modifyMVar_ (pDataVar pool) $ \pd -> do
          case Queue.pop (pdQueue pd) of
            Just (receptor, qu') -> do
              putMVar receptor newWorker
              return pd { pdQueue = qu' }
            Nothing ->
              return pd { pdAvailable = newWorker : pdAvailable pd }
      return result

forceString :: String -> String
forceString = foldr seq >>= id

duration :: IO a -> IO (Double, a)
duration action = do
  starttm <- Clock.getTime Clock.Monotonic
  res <- action
  endtm <- Clock.getTime Clock.Monotonic
  let diff = Clock.diffTimeSpec starttm endtm
      secs = fromIntegral (Clock.sec diff) + fromIntegral (Clock.nsec diff) / 1e9
  return (secs, res)

terminateParanoid :: Pr.ProcessHandle -> IO ()
terminateParanoid ph = do
  Pr.terminateProcess ph
  -- Wait 50ms to let the process exit somewhat cleanly (more cleanly than SIGKILL, at least)
  mec <- timeout 50_000 $ Pr.waitForProcess ph
  case mec of
    Just _ -> return ()
    Nothing -> do
      putStrLn "(Had to SIGKILL process...)"
      PrI.withProcessHandle ph $ \case
        PrI.OpenHandle pid -> signalProcess sigKILL pid
        PrI.OpenExtHandle pid _ -> signalProcess sigKILL pid
        PrI.ClosedHandle _ -> return ()
