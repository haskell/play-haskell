{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
module GHCPool (
  availableVersions,
  Command(..),
  Version(..),
  Pool,
  makePool,
  runInPool,
) where

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Char (isDigit)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStr, hGetContents, hClose)
import System.Posix.Directory (getWorkingDirectory)
import qualified System.Process as Pr
import System.Timeout (timeout)

import Data.Queue (Queue)
import qualified Data.Queue as Queue


ghcupHomeDir :: IO String
ghcupHomeDir = getEnv "HOME"

runTimeoutMicrosecs :: Int
runTimeoutMicrosecs = 5_000_000

availableVersions :: IO [String]
availableVersions = do
  homedir <- ghcupHomeDir
  files <- listDirectory (homedir </> ".ghcup" </> "bin")
  return [version
         | f <- files
         , let fname = takeFileName f
         , let (prefix, version) = splitAt 4 fname
         , prefix == "ghc-"
         , all (\c -> isDigit c || c == '.') version
         , filter (== '.') version == ".."]

data Command = CRun
  deriving (Show)

commandString :: Command -> String
commandString CRun = "run"

newtype Version = Version String deriving (Show)

data RunResult = TimeOut
               | Finished ExitCode String String  -- ^ stdin stdout
  deriving (Show)

data Worker = Worker ThreadId
                     (MVar (Command, Version, String))  -- ^ input
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
    homedir <- ghcupHomeDir
    workdir <- getWorkingDirectory
    let spec = (Pr.proc (workdir </> "bwrap-files/start.sh") [homedir])
                  { Pr.std_in = Pr.CreatePipe
                  , Pr.std_out = Pr.CreatePipe
                  , Pr.std_err = Pr.CreatePipe }
    Pr.withCreateProcess spec $ \(Just inh) (Just outh) (Just errh) proch -> do
      (cmd, Version ver, source) <- readMVar mvar
      _ <- forkIO $ do
        hPutStr inh (commandString cmd ++ "\n" ++ ver ++ "\n" ++ source)
        hClose inh
      stdoutmvar <- newEmptyMVar
      _ <- forkIO $ hGetContents outh >>= evaluate . forceString >>= putMVar stdoutmvar
      stderrmvar <- newEmptyMVar
      _ <- forkIO $ hGetContents errh >>= evaluate . forceString >>= putMVar stderrmvar
      mec <- timeout runTimeoutMicrosecs $ Pr.waitForProcess proch
      case mec of
        Just ec -> do
          out <- readMVar stdoutmvar
          err <- readMVar stderrmvar
          putMVar resultvar (Finished ec out err)
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

runInPool :: Pool -> Command -> Version -> String -> IO (Either String (ExitCode, String, String))
runInPool pool cmd ver source = do
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
    QueueFull -> return (Left "The queue is currently full, try again later")
  where
    useWorker :: Worker -> IO (Either String (ExitCode, String, String))
    useWorker (Worker _tid invar outvar) = do
      putMVar invar (cmd, ver, source)
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
        Finished ex out err -> return (Right (ex, out, err))
        TimeOut -> return (Left "Running your code resulted in a timeout")

forceString :: String -> String
forceString = foldr seq >>= id
