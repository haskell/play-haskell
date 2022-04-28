{-# OPTIONS -Wno-orphans #-}
module PlayHaskellTypes (
  -- * Supporting types
  Command(..),
  Optimisation(..),
  Version(..),
  RunError(..),

  -- * Request/response types
  RunRequest(..),
  RunResponse(..),
) where

import System.Exit
import Text.JSON


data Command = CRun   -- ^ Compile and execute a Haskell file
             | CCore  -- ^ Get simplified core
             | CAsm   -- ^ Get native assembly
  deriving (Show)

data Optimisation = O0 | O1 | O2
  deriving (Show, Read)

-- | The GHC version to use for a job.
newtype Version = Version String
  deriving (Show)

data RunError = RETimeOut
  deriving (Show)

-- | Body payload for a request to a worker for running a particular piece of
-- source code.
data RunRequest = RunRequest
  { runreqCommand :: Command
  , runreqSource :: String
  , runreqVersion :: Version
  , runreqOpt :: Optimisation
  , runreqChallenge :: String }
  deriving (Show)

-- | Body payload for a response to a 'RunRequest'.
data RunResponse
  = RunResponseErr RunError
  | RunResponseOk
      { runresExitCode :: ExitCode
      , runresStdout :: String
      , runresStderr :: String
      , runresTimeTakenSecs :: Double }
  deriving (Show)

instance JSON Command where
  readJSON (JSString s) = case fromJSString s of
    "run" -> return CRun ; "core" -> return CCore ; "asm" -> return CAsm ;
    _ -> fail "Unable to read Command"
  readJSON _ = fail "Unable to read Command"

  showJSON cmd = JSString $ toJSString $ case cmd of
    CRun -> "run" ; CCore -> "core" ; CAsm -> "asm"

instance JSON Optimisation where
  readJSON (JSString s) = case fromJSString s of
    "O0" -> return O0 ; "O1" -> return O1 ; "O2" -> return O2 ;
    _ -> fail "Unable to read Optimisation"
  readJSON _ = fail "Unable to read Optimisation"

  showJSON cmd = JSString $ toJSString $ case cmd of
    O0 -> "O0" ; O1 -> "O1" ; O2 -> "O2"

instance JSON Version where
  readJSON (JSString s) = return (Version (fromJSString s))
  readJSON _ = fail "Unable to read Version"

  showJSON (Version s) = JSString (toJSString s)

instance JSON RunError where
  readJSON (JSString s) = case fromJSString s of
                            "timeout" -> return RETimeOut
                            _ -> fail "Unable to read RunError"
  readJSON _ = fail "Unable to read RunError"

  showJSON RETimeOut = JSString (toJSString "timeout")

instance JSON RunRequest where
  readJSON (JSObject obj) =
    case sequence (map (`lookup` fromJSObject obj)
                       ["cmd", "src", "ver", "opt", "chal"]) of
      Just [cmd, src, ver, opt, chal] ->
        RunRequest <$> readJSON cmd
                   <*> (fromJSString <$> readJSON src)
                   <*> readJSON ver
                   <*> readJSON opt
                   <*> (fromJSString <$> readJSON chal)
      _ -> fail "Unable to read RunRequest"
  readJSON _ = fail "Unable to read RunRequest"

  showJSON (RunRequest cmd src ver opt chal) =
    JSObject $ toJSObject
      [("cmd", showJSON cmd)
      ,("src", showJSON (toJSString src))
      ,("ver", showJSON ver)
      ,("opt", showJSON opt)
      ,("chal", showJSON (toJSString chal))]

instance JSON RunResponse where
  readJSON (JSObject obj) =
    case lookup "err" (fromJSObject obj) of
      Just err -> RunResponseErr <$> readJSON err
      Nothing ->
        case sequence (map (`lookup` fromJSObject obj)
                           ["cmd", "src", "ver", "opt", "chal"]) of
          Just [ec, sout, serr, timesecs] ->
            RunResponseOk <$> readJSON ec
                          <*> (fromJSString <$> readJSON sout)
                          <*> (fromJSString <$> readJSON serr)
                          <*> readJSON timesecs
          _ -> fail "Unable to read RunResponse"
  readJSON _ = fail "Unable to read RunResponse"

  showJSON (RunResponseErr err) = JSObject $ toJSObject [("err", showJSON err)]
  showJSON (RunResponseOk ec sout serr timesecs) =
    JSObject $ toJSObject
      [("ec", showJSON ec)
      ,("sout", showJSON (toJSString sout))
      ,("serr", showJSON (toJSString serr))
      ,("timesecs", showJSON timesecs)]

instance JSON ExitCode where
  readJSON value = do
    code <- readJSON value :: Result Int
    case code of 0 -> return ExitSuccess
                 _ -> return (ExitFailure code)

  showJSON ExitSuccess = showJSON (0 :: Int)
  showJSON (ExitFailure code) = showJSON code
