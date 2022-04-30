{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -Wno-orphans #-}
module PlayHaskellTypes (
  -- * Supporting types
  Command(..),
  Optimisation(..),
  Version(..),
  RunError(..),

  -- * Request/response types
  Message(..),
  signMessage,
  RunRequest(..),
  RunResponse(..),

  -- * Signing utilities
  SigningBytes(..),
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as Lazy
import Data.Int (Int64)
import Data.Word (Word64)
import System.Exit
import Text.JSON

import PlayHaskellTypes.Sign (PublicKey, SecretKey, Signature)
import qualified PlayHaskellTypes.Sign as Sign


-- | JSON: string; "run", "core", "asm".
data Command = CRun   -- ^ Compile and execute a Haskell file
             | CCore  -- ^ Get simplified core
             | CAsm   -- ^ Get native assembly
  deriving (Show)

-- | JSON: string; \"O0", \"O1", \"O2".
data Optimisation = O0 | O1 | O2
  deriving (Show, Read)

-- | The GHC version to use for a job.
--
-- JSON: string.
newtype Version = Version String
  deriving (Show)

-- | JSON: string; "timeout".
data RunError = RETimeOut
  deriving (Show)

-- | A signed message. The bytes being signed are the 'signingBytes' of the content.
--
-- JSON: object; "sig", "pkey", "con".
data Message a = Message
  { sesmsgSignature :: Signature  -- ^ Signature of the content with the secret key of the sender.
  , sesmsgPublicKey :: PublicKey  -- ^ Public key of the sender.
  , sesmsgContent :: a            -- ^ Message content.
  }
  deriving (Show)

-- | Body payload for a request to a worker for running a particular piece of
-- source code.
--
-- JSON: object; "cmd", "src", "ver", "opt".
data RunRequest = RunRequest
  { runreqCommand :: Command
  , runreqSource :: String
  , runreqVersion :: Version
  , runreqOpt :: Optimisation }
  deriving (Show)

-- | Body payload for a response to a 'RunRequest'.
--
-- JSON: object; either "err" or "ec", "sout", "serr", "timesecs".
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

instance JSON a => JSON (Message a) where
  readJSON (JSObject obj) =
    case mapM (`lookup` fromJSObject obj) ["sig", "pkey", "con"] of
      Just [sig, pkey, con] ->
        Message <$> readJSON sig
                <*> readJSON pkey
                <*> readJSON con
      _ -> fail "Unable to read Message"
  readJSON _ = fail "Unable to read Message"

  showJSON (Message sig pkey con) =
    JSObject $ toJSObject
      [("sig", showJSON sig)
      ,("pkey", showJSON pkey)
      ,("con", showJSON con)]

instance JSON RunRequest where
  readJSON (JSObject obj) =
    case mapM (`lookup` fromJSObject obj) ["cmd", "src", "ver", "opt"] of
      Just [cmd, src, ver, opt] ->
        RunRequest <$> readJSON cmd
                   <*> (fromJSString <$> readJSON src)
                   <*> readJSON ver
                   <*> readJSON opt
      _ -> fail "Unable to read RunRequest"
  readJSON _ = fail "Unable to read RunRequest"

  showJSON (RunRequest cmd src ver opt) =
    JSObject $ toJSObject
      [("cmd", showJSON cmd)
      ,("src", showJSON (toJSString src))
      ,("ver", showJSON ver)
      ,("opt", showJSON opt)]

instance JSON RunResponse where
  readJSON (JSObject obj) =
    case lookup "err" (fromJSObject obj) of
      Just err -> RunResponseErr <$> readJSON err
      Nothing ->
        case mapM (`lookup` fromJSObject obj) ["ec", "sout", "serr", "timesecs"] of
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

-- | Encoded as an integer, with 0 mapping to 'ExitSuccess'.
instance JSON ExitCode where
  readJSON value = do
    code <- readJSON value :: Result Int
    case code of 0 -> return ExitSuccess
                 _ -> return (ExitFailure code)

  showJSON ExitSuccess = showJSON (0 :: Int)
  showJSON (ExitFailure code) = showJSON code

-- | Build and sign a message using your secret key.
signMessage :: SigningBytes a => SecretKey -> a -> Message a
signMessage skey content =
  Message { sesmsgSignature = Sign.sign skey (signingBytes content)
          , sesmsgPublicKey = Sign.publicKey skey
          , sesmsgContent = content }

class SigningBytes a where
  -- | The result should be prefix-unambiguous: given a 'ByteString' that is
  -- the result of 'signingBytes' plus some arbitrary content, it must be
  -- unambiguous exactly what prefix corresponds to the type @a@, and what @a@
  -- value it represents.
  signingBytesB :: a -> Builder

  -- | @'signingBytes' = 'Lazy.toStrict' . 'BSB.toLazyByteString' . 'signingBytesB'@
  signingBytes :: a -> ByteString
  signingBytes = Lazy.toStrict . BSB.toLazyByteString . signingBytesB

instance SigningBytes ByteString where
  signingBytesB bs = BSB.word64LE (fromIntegral @Int @Word64 (BS.length bs)) <> BSB.byteString bs

newtype SigningBytesUTF8String = SigningBytesUTF8String String

instance SigningBytes SigningBytesUTF8String where
  signingBytesB (SigningBytesUTF8String s) =
    BSB.word64LE (fromIntegral @Int @Word64 (length s)) <> BSB.stringUtf8 s

instance SigningBytes Command where
  signingBytesB CRun = BSB.word8 0
  signingBytesB CCore = BSB.word8 1
  signingBytesB CAsm = BSB.word8 2

instance SigningBytes Optimisation where
  signingBytesB O0 = BSB.word8 0
  signingBytesB O1 = BSB.word8 1
  signingBytesB O2 = BSB.word8 2

deriving via SigningBytesUTF8String instance SigningBytes Version

instance SigningBytes RunError where
  signingBytesB RETimeOut = BSB.word8 0

instance SigningBytes RunRequest where
  signingBytesB (RunRequest cmd src ver opt) =
    mconcat [signingBytesB cmd
            ,signingBytesB (SigningBytesUTF8String src)
            ,signingBytesB ver
            ,signingBytesB opt]

instance SigningBytes RunResponse where
  signingBytesB (RunResponseErr err) =
    mconcat [BSB.word8 0
            ,signingBytesB err]
  signingBytesB (RunResponseOk ec sout serr timesecs) =
    mconcat [BSB.word8 1
            ,signingBytesB ec
            ,signingBytesB (SigningBytesUTF8String sout)
            ,signingBytesB (SigningBytesUTF8String serr)
            ,BSB.doubleLE timesecs]

instance SigningBytes ExitCode where
  signingBytesB ExitSuccess = BSB.int64LE 0
  signingBytesB (ExitFailure ec) = BSB.int64LE (fromIntegral @Int @Int64 ec)
