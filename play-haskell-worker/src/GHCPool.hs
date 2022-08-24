{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module GHCPool (
  availableVersions,
  Pool,
  makePool,
  Result(..),
  runInPool,
  poolParallelism,
) where

import Control.Concurrent
import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Foreign.Marshal.Alloc (allocaBytes)
import qualified System.Clock as Clock
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStr, hClose, Handle, hGetBufSome)
import System.Posix.Directory (getWorkingDirectory)
import System.Posix.IO (createPipe, fdToHandle, closeFd)
import System.Posix.Signals (signalProcess, sigKILL)
import qualified System.Process as Pr
import qualified System.Process.Internals as PrI
import System.Timeout (timeout)

import Data.Queue (Queue)
import qualified Data.Queue as Queue
import PlayHaskellTypes (Command(..), Optimisation(..), Version(..), RunError(..))
import PlayHaskellTypes.Constants


maxOutputSizeBytes :: Int
maxOutputSizeBytes = 100_000

availableVersions :: IO [String]
availableVersions = do
  -- -r: parseable output
  -- --no-verbose: don't notify me that there is a new ghc version available
  out <- Pr.readCreateProcess (Pr.proc "ghcup" ["--offline", "list", "-t", "ghc", "-c", "installed", "-r", "--no-verbose"]) []
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
  , resGhcOut :: Lazy.ByteString
  , resStdout :: Lazy.ByteString
  , resStderr :: Lazy.ByteString
  , resTimeTaken :: Double  -- ^ seconds
  }
  deriving (Show)

type RunResult = Either RunError Result

data Worker = Worker ThreadId
                     (MVar (Command, Optimisation, Version, Text))  -- ^ input
                     (MVar RunResult)  -- ^ output

data PoolData = PoolData
  { pdAvailable :: [Worker]
  , pdQueue :: Queue (MVar Worker) }

data Pool = Pool { pDataVar :: MVar PoolData
                 , pNumWorkers :: Int }

makeWorker :: IO Worker
makeWorker = do
  mvar <- newEmptyMVar
  resultvar <- newEmptyMVar
  thread <- forkIO $ do
    workdir <- getWorkingDirectory
    -- Create a pipe that GHC output will be written to
    (ghcOutReadFD, ghcOutWriteFD) <- createPipe
    ghcOutReadHandle <- fdToHandle ghcOutReadFD
    let spec = (Pr.proc (workdir </> "bwrap-files/stage-1.sh") [show ghcOutWriteFD])
                  { Pr.std_in = Pr.CreatePipe
                  , Pr.std_out = Pr.CreatePipe
                  , Pr.std_err = Pr.CreatePipe }
    Pr.withCreateProcess spec $ \(Just inh) (Just outh) (Just errh) proch -> do
      -- Make sure our copy of the writing end of the pipe is closed, so that the pipe gets closed when the process is done
      closeFd ghcOutWriteFD
      (cmd, opt, Version ver, source) <- readMVar mvar
      _ <- forkIO $ do
        hPutStr inh (commandString cmd ++ "\n" ++ optimisationString opt ++ "\n" ++ ver ++ "\n")
        BS.hPutStr inh (TE.encodeUtf8 source)
        hClose inh
      ghcoutmvar <- newEmptyMVar
      readTh1 <- forkIO $ hGetContentsUTF8Bounded maxOutputSizeBytes ghcOutReadHandle >>= putMVar ghcoutmvar
      stdoutmvar <- newEmptyMVar
      readTh2 <- forkIO $ hGetContentsUTF8Bounded maxOutputSizeBytes outh >>= putMVar stdoutmvar
      stderrmvar <- newEmptyMVar
      readTh3 <- forkIO $ hGetContentsUTF8Bounded maxOutputSizeBytes errh >>= putMVar stderrmvar
      (dur, mec) <- duration $ timeout runTimeoutMicrosecs $ Pr.waitForProcess proch
      case mec of
        Just ec -> do
          ghcout <- readMVar ghcoutmvar
          out <- readMVar stdoutmvar
          err <- readMVar stderrmvar
          putMVar resultvar (Right (Result ec ghcout out err dur))
        Nothing -> do
          mapM_ killThread [readTh1, readTh2, readTh3]
          -- Paranoid termination is technically unnecessary since bwrap seems
          -- to kill its child with SIGKILL if it is itself killed using
          -- SIGTERM, which is what Pr.terminateProcess sends. However, let's
          -- be safe.
          terminateParanoid proch
          putMVar resultvar (Left RETimeOut)
  return (Worker thread mvar resultvar)

-- | makePool numWorkers
makePool :: Int -> IO Pool
makePool numWorkers = do
  -- putStrLn $ "Making a GHCPool with numWorkers=" ++ show numWorkers
  workers <- replicateM numWorkers makeWorker
  let pd = PoolData { pdAvailable = workers
                    , pdQueue = Queue.empty }
  pdvar <- newMVar pd
  return (Pool pdvar numWorkers)

data ObtainedWorker = Obtained Worker
                    | Queued (MVar Worker) Int

runInPool :: Pool -> Command -> Version -> Optimisation -> Text -> IO (Either RunError Result)
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

poolParallelism :: Pool -> Int
poolParallelism = pNumWorkers

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
