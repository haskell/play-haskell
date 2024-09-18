{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- I'm using unsafePerformIO to create a global mutex. I believe this is
-- necessary to let that go alright.
{-# OPTIONS -fno-full-laziness -fno-cse #-}

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
import Foreign.C.Error (eFAULT, Errno (Errno))
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO.Exception (IOException(ioe_errno))
import qualified System.Clock as Clock
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStr, hClose, Handle, hGetBufSome, hPutStrLn, stderr)
import System.IO.Error (catchIOError)
import System.Posix.Directory (getWorkingDirectory)
import System.Posix.IO (createPipe, fdToHandle, closeFd)
import qualified System.Process as Pr
import System.Timeout (timeout)

import System.IO.Unsafe (unsafePerformIO)

import Data.Queue (Queue)
import qualified Data.Queue as Queue
import PlayHaskellTypes (Command(..), Optimisation(..), Version(..), RunError(..))
import PlayHaskellTypes.Constants


{-# NOINLINE debugMutex #-}
debugMutex :: MVar ()
debugMutex = unsafePerformIO $ newMVar ()

debug :: String -> IO ()
debug str = do
  () <- takeMVar debugMutex
  hPutStrLn stderr str
  putMVar debugMutex ()

maxOutputSizeBytes :: Int
maxOutputSizeBytes = 100_000

availableVersions :: IO [String]
availableVersions = do
  files <- listDirectory "bwrap-files/builders"
  return [ver
         | fname <- files
         , ("build-", tl) <- [splitAt 6 fname]
         , (ver, ".sh") <- [splitAt (length tl - 3) tl]]

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
    withCreateProcessRetry spec $ \(Just inh) (Just outh) (Just errh) proch -> do
      -- debug $ "Created new stage-1 process: inh=" ++ show inh ++ " outh=" ++ show outh ++ " errh=" ++ show errh ++ " ghcoutread=" ++ show ghcOutReadHandle
      -- Make sure our copy of the writing end of the pipe is closed, so that the pipe gets closed when the process is done
      closeFd ghcOutWriteFD
      (cmd, opt, Version ver, source) <- readMVar mvar
      _ <- forkIO $ do
        hPutStr inh (commandString cmd ++ "\n" ++ optimisationString opt ++ "\n" ++ ver ++ "\n")
        BS.hPutStr inh (TE.encodeUtf8 source)
        hClose inh
      ghcoutmvar <- newEmptyMVar
      readTh1 <- forkIO $ hGetContentsBounded maxOutputSizeBytes ghcOutReadHandle >>= putMVar ghcoutmvar
      stdoutmvar <- newEmptyMVar
      readTh2 <- forkIO $ hGetContentsBounded maxOutputSizeBytes outh >>= putMVar stdoutmvar
      stderrmvar <- newEmptyMVar
      readTh3 <- forkIO $ hGetContentsBounded maxOutputSizeBytes errh >>= putMVar stderrmvar
      -- debug $ "[pool] Waiting for process"
      (dur, mec) <- duration $ timeout runTimeoutMicrosecs $ Pr.waitForProcess proch
      -- debug $ "[pool] done with " ++ show (dur, mec)
      case mec of
        Just ec -> do
          -- debug $ "[pool] waiting on ghc out fd " ++ show ghcOutReadFD
          ghcout <- readMVar ghcoutmvar
          -- debug $ "[pool] ghc out done len=" ++ show (Lazy.length ghcout)
          out <- readMVar stdoutmvar
          -- debug $ "[pool] out done len=" ++ show (Lazy.length out)
          err <- readMVar stderrmvar
          -- debug $ "[pool] err done len=" ++ show (Lazy.length err)
          putMVar resultvar (Right (Result ec ghcout out err dur))
        Nothing -> do
          mapM_ killThread [readTh1, readTh2, readTh3]
          -- debug $ "[pool] killed read threads for timeouted process"
          -- mpid <- Pr.getPid proch

          -- Note that this just kills the stage-1 script with SIGTERM. (The fact that it
          -- does is an implementation detail of the 'process' library...) This should be
          -- sufficient (i.e. we don't have to resort to SIGKILL) because stage-1 doesn't
          -- itself run user code, it just starts the systemd unit and is thus still
          -- known/trusted code. And we know that stage-1 will kill the systemd unit as it
          -- should upon SIGTERM, and it won't if we SIGKILL it. Hence we unfortunately
          -- cannot SIGKILL as a second-layer defense.
          -- debug $ "[pool] terminateParanoid: terminating " ++ show mpid
          Pr.terminateProcess proch
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

-- | The passed 'Int' is the maximum number of bytes read. The rest of the
-- handle is consumed but not stored.
hGetContentsBounded :: Int -> Handle -> IO Lazy.ByteString
hGetContentsBounded bound h = do
  let bufsize = 16 * 1024
  builder <- allocaBytes bufsize $ \ptr -> do
    let loop numleft
          | numleft <= 0 = do
              -- read everything there is to read without storing it
              nread <- hGetBufSome h ptr bufsize
              -- debug $ "hGCU8B(" ++ show h ++ "): read " ++ show nread ++ " excess bytes"
              if nread == 0 then return mempty else loop 0
          | otherwise = do
              nread <- hGetBufSome h ptr bufsize
              -- debug $ "hGCU8B(" ++ show h ++ "): read " ++ show nread ++ " bytes"
              if nread == 0
                then do return mempty
                else do let numprocessed = min nread numleft
                        bs <- BS.packCStringLen (ptr, numprocessed)
                        (BSB.byteString bs <>) <$> loop (numleft - numprocessed)
    loop bound
  return (BSB.toLazyByteString builder)

withCreateProcessRetry
  :: Pr.CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> Pr.ProcessHandle -> IO a)
  -> IO a
withCreateProcessRetry spec f = do
  res <- catchIOError
    (Pr.withCreateProcess spec $ \minh mouth merrh proch -> do
       Just <$> f minh mouth merrh proch)
    (\ioe -> let Errno efault = eFAULT
             in if ioe_errno ioe == Just efault
                  then do debug "Oops, EFAULT!"
                          return Nothing
                  else ioError ioe)

  case res of
    Just r -> return r
    Nothing -> withCreateProcessRetry spec f
