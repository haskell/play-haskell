{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
module GHCPool (
  availableVersions,
  runTimeoutMicrosecs,
  Command(..),
  Optimization(..),
  Version(..),
  Pool,
  makePool,
  Result(..),
  RunPoolError(..),
  runInPool,
) where

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Char (isDigit)
import Data.List (sort)
import qualified System.Clock as Clock
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStr, hGetContents, hClose)
import System.Posix.Directory (getWorkingDirectory)
import qualified System.Process as Pr
import System.Timeout (timeout)
import Safe
import Data.Maybe

import Data.Queue (Queue)
import qualified Data.Queue as Queue


runTimeoutMicrosecs :: Int
runTimeoutMicrosecs = 5_000_000

availableVersions :: IO [String]
availableVersions = do
  out <- Pr.readCreateProcess (Pr.proc "ghcup" ["--offline", "list", "-t", "ghc", "-c", "installed", "-r"]) []
  let ghc_versions = catMaybes $ fmap (`atMay` 1) $ fmap words $ lines out
  return ghc_versions

data Command = CRun
             | CCore
             | CAsm
  deriving (Show)

commandString :: Command -> String
commandString CRun = "run"
commandString CCore = "core"
commandString CAsm = "asm"

data Optimization = O0
                  | O1
                  | O2
  deriving (Show, Read)

optimizationString :: Optimization -> String
optimizationString O0 = "-O0"
optimizationString O1 = "-O1"
optimizationString O2 = "-O2"

newtype Version = Version String deriving (Show)

data Result = Result
  { resExitCode :: ExitCode
  , resStdout :: String
  , resStderr :: String
  , resTimeTaken :: Double  -- ^ seconds
  }
  deriving (Show)

data RunResult = TimeOut | Finished Result
  deriving (Show)

data Worker = Worker ThreadId
                     (MVar (Command, Optimization, Version, String))  -- ^ input
                     (MVar RunResult)  -- ^ output

data PoolData = PoolData
  { pdAvailable :: [Worker]
  , pdQueue :: Queue (MVar Worker) }

data Pool = Pool { pDataVar :: MVar PoolData
                 , pMaxQueueLen :: Int }

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
        hPutStr inh (commandString cmd ++ "\n" ++ optimizationString opt ++ "\n" ++ ver ++ "\n" ++ source)
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
          putMVar resultvar (Finished (Result ec out err dur))
        Nothing -> do
          Pr.terminateProcess proch
          -- TODO: do we need to SIGKILL as well?
          putMVar resultvar TimeOut
  return (Worker thread mvar resultvar)

-- | makePool numWorkers maxQueueLen
makePool :: Int -> Int -> IO Pool
makePool numWorkers maxQueueLen = do
  workers <- replicateM numWorkers makeWorker
  let pd = PoolData { pdAvailable = workers
                    , pdQueue = Queue.empty }
  pdvar <- newMVar pd
  return (Pool pdvar maxQueueLen)

data ObtainedWorker = Obtained Worker
                    | Queued (MVar Worker)
                    | QueueFull

data RunPoolError = EQueueFull
                  | ETimeOut

runInPool :: Pool -> Command -> Version -> Optimization -> String -> IO (Either RunPoolError Result)
runInPool pool cmd ver opt source = do
  result <- modifyMVar (pDataVar pool) $ \pd ->
              case pdAvailable pd of
                w:ws ->
                  return (pd { pdAvailable = ws }, Obtained w)
                [] | Queue.size (pdQueue pd) <= pMaxQueueLen pool -> do
                       receptor <- newEmptyMVar
                       return (pd { pdQueue = Queue.push (pdQueue pd) receptor }
                              ,Queued receptor)
                   | otherwise ->
                       return (pd, QueueFull)

  case result of
    Obtained worker -> useWorker worker
    Queued receptor -> readMVar receptor >>= useWorker
    QueueFull -> return (Left EQueueFull)
  where
    useWorker :: Worker -> IO (Either RunPoolError Result)
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
      case result of
        Finished res -> return (Right res)
        TimeOut -> return (Left ETimeOut)

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
