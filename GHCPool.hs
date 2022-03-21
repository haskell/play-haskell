{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
module GHCPool where

import Control.Concurrent
import Control.Monad (replicateM)
import Data.Char (isDigit)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStr, hGetContents)
import System.Posix.Directory (getWorkingDirectory)
import qualified System.Process as Pr
import System.Timeout (timeout)


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

data Worker = Worker ThreadId
                     (MVar (Command, Version, String))  -- ^ input
                     (MVar (Maybe (ExitCode, String, String)))  -- ^ output

data PoolData = PoolData
  { pdAvailable :: [Worker] }

data Pool = Pool (MVar PoolData)

bwrapCommand :: String -> String -> (String, [String])
bwrapCommand homedir workdir =
  ("bwrap"
  ,["--ro-bind", "/bin", "/bin"
   ,"--ro-bind", "/usr/bin", "/usr/bin"
   ,"--ro-bind", "/usr/lib", "/usr/lib"
   ,"--ro-bind", "/lib", "/lib"
   ,"--ro-bind", "/lib64", "/lib64"
   ,"--ro-bind", "/etc", "/etc"
   ,"--ro-bind", homedir ++ "/.ghcup/bin", homedir ++ "/.ghcup/bin"
   ,"--ro-bind", homedir ++ "/.ghcup/ghc", homedir ++ "/.ghcup/ghc"
   ,"--dev", "/dev"
   ,"--proc", "/proc"
   ,"--bind", workdir ++ "/bwrap-workdir", homedir ++ "/workdir"
   ,homedir ++ "/workdir/entry.sh"])

makeWorker :: IO Worker
makeWorker = do
  mvar <- newEmptyMVar
  resultvar <- newEmptyMVar
  thread <- forkIO $ do
    homedir <- ghcupHomeDir
    workdir <- getWorkingDirectory
    let (bwrapcmd, args) = bwrapCommand homedir workdir
        spec = (Pr.proc bwrapcmd args)
                  { Pr.std_in = Pr.CreatePipe
                  , Pr.std_out = Pr.CreatePipe
                  , Pr.std_err = Pr.CreatePipe }
    Pr.withCreateProcess spec $ \(Just inh) (Just outh) (Just errh) proch -> do
      (cmd, Version ver, source) <- readMVar mvar
      _ <- forkIO $ hPutStr inh (commandString cmd ++ "\n" ++ ver ++ "\n" ++ source)
      stdoutmvar <- newEmptyMVar
      _ <- forkIO $ hGetContents outh >>= putMVar stdoutmvar
      stderrmvar <- newEmptyMVar
      _ <- forkIO $ hGetContents errh >>= putMVar stderrmvar
      mec <- timeout runTimeoutMicrosecs $ Pr.waitForProcess proch
      case mec of
        Just ec ->
          (ec,,) <$> readMVar stdoutmvar <*> readMVar stderrmvar
            >>= putMVar resultvar . Just
        Nothing -> do
          Pr.terminateProcess proch
          -- TODO: do we need to SIGKILL as well?
          putMVar resultvar Nothing
  return (Worker thread mvar resultvar)

makePool :: Int -> IO Pool
makePool numWorkers = do
  workers <- replicateM numWorkers makeWorker
  let pd = PoolData { pdAvailable = workers }
  Pool <$> newMVar pd

runInPool :: Pool -> Command -> Version -> String -> IO (Either String (ExitCode, String, String))
runInPool (Pool pdvar) cmd ver source = do
  mworker <- modifyMVar pdvar $ \pd ->
               case pdAvailable pd of
                 [] -> return (pd, Nothing)
                 w:ws -> return (pd { pdAvailable = ws }, Just w)
  case mworker of
    Just (Worker _tid invar outvar) -> do
      putMVar invar (cmd, ver, source)
      result <- readMVar outvar
      _ <- forkIO $ do
        newWorker <- makeWorker
        modifyMVar_ pdvar $ \pd -> return pd { pdAvailable = newWorker : pdAvailable pd }
      case result of
        Just res -> return (Right res)
        Nothing -> return (Left "Running your code resulted in a timeout")
    Nothing ->
      -- TODO queueing
      return (Left "No workers available, try again later")
