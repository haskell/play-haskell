{-# LANGUAGE LambdaCase #-}
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
import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import Foreign.Marshal.Alloc (allocaBytes)
import qualified System.Clock as Clock
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStr, hClose, Handle, hGetBufSome)
import System.Posix.Directory (getWorkingDirectory)
import System.Posix.Signals (signalProcess, sigKILL)
import qualified System.Process as Pr
import qualified System.Process.Internals as PrI
import System.Timeout (timeout)
import Safe
import Data.Maybe

import Data.Queue (Queue)
import qualified Data.Queue as Queue


runTimeoutMicrosecs :: Int
runTimeoutMicrosecs = 5_000_000

maxOutputSizeBytes :: Int
maxOutputSizeBytes = 100_000

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
      _ <- forkIO $ hGetContentsUTF8Bounded maxOutputSizeBytes outh >>= putMVar stdoutmvar . LUTF8.toString
      stderrmvar <- newEmptyMVar
      _ <- forkIO $ hGetContentsUTF8Bounded maxOutputSizeBytes errh >>= putMVar stderrmvar . LUTF8.toString
      (dur, mec) <- duration $ timeout runTimeoutMicrosecs $ Pr.waitForProcess proch
      case mec of
        Just ec -> do
          out <- readMVar stdoutmvar
          err <- readMVar stderrmvar
          putMVar resultvar (Finished (Result ec out err dur))
        Nothing -> do
          -- Paranoid termination is technically unnecessary since bwrap seems
          -- to kill its child with SIGKILL if it is itself killed using
          -- SIGTERM, which is what Pr.terminateProcess sends. However, let's
          -- be safe.
          terminateParanoid proch
          putMVar resultvar TimeOut
  return (Worker thread mvar resultvar)

-- | makePool numWorkers maxQueueLen
makePool :: Int -> Int -> IO Pool
makePool numWorkers maxQueueLen = do
  -- putStrLn $ "Making a GHCPool with numWorkers=" ++ show numWorkers ++ " maxQueueLen=" ++ show maxQueueLen
  workers <- replicateM numWorkers makeWorker
  let pd = PoolData { pdAvailable = workers
                    , pdQueue = Queue.empty }
  pdvar <- newMVar pd
  return (Pool pdvar maxQueueLen)

data ObtainedWorker = Obtained Worker
                    | Queued (MVar Worker) Int
                    | QueueFull

data RunPoolError = EQueueFull
                  | ETimeOut

runInPool :: Pool -> Command -> Version -> Optimization -> String -> IO (Either RunPoolError Result)
runInPool pool cmd ver opt source = do
  result <- modifyMVar (pDataVar pool) $ \pd ->
              case pdAvailable pd of
                w:ws ->
                  return (pd { pdAvailable = ws }, Obtained w)
                [] | Queue.size (pdQueue pd) < pMaxQueueLen pool -> do
                       receptor <- newEmptyMVar
                       let newq = Queue.push (pdQueue pd) receptor
                       return (pd { pdQueue = newq }
                              ,Queued receptor (Queue.size newq))
                   | otherwise ->
                       return (pd, QueueFull)

  case result of
    Obtained worker -> do -- putStrLn "[obtained]"
                          useWorker worker
    Queued receptor _newlen -> do -- putStrLn ("[queued len=" ++ show newlen ++ "]")
                                  readMVar receptor >>= useWorker
    QueueFull -> do -- putStrLn "[queue full]"
                    return (Left EQueueFull)
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

-- | The passed 'Int' is the maximum number of bytes read. The rest of the
-- handle is consumed but not stored.
hGetContentsUTF8Bounded :: Int -> Handle -> IO Lazy.ByteString
hGetContentsUTF8Bounded bound h = do
  let bufsize = 16 * 1024
  builder <- allocaBytes bufsize $ \ptr -> do
    let loop numleft
          | numleft <= 0 = do
              -- read everything there is to read without storing it
              nread <- hGetBufSome h ptr bufsize
              if nread == 0 then return mempty else loop 0
          | otherwise = do
              nread <- hGetBufSome h ptr bufsize
              if nread == 0
                then do return mempty
                else do let numprocessed = min nread numleft
                        bs <- BS.packCStringLen (ptr, numprocessed)
                        (BSB.byteString bs <>) <$> loop (numleft - numprocessed)
    loop bound
  return (BSB.toLazyByteString builder)
