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
  VersionsResponse(..),

  -- * Signing utilities
  SigningBytes(..),
) where

import Control.Applicative ((<|>))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as Lazy
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import System.Exit

import PlayHaskellTypes.Sign (PublicKey, SecretKey, Signature)
import qualified PlayHaskellTypes.Sign as Sign
import PlayHaskellTypes.UTF8


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
  deriving (Show, Eq, Ord)

-- | JSON: string; "timeout", "backend".
data RunError = RETimeOut
              | REBackend
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
  , runreqSource :: Text
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
      , runresStdout :: Lazy.ByteString
      , runresStderr :: Lazy.ByteString
      , runresTimeTakenSecs :: Double }
  deriving (Show)

newtype VersionsResponse = VersionsResponse [Version]
  deriving (Show)
  deriving (J.ToJSON, J.FromJSON) via [Version]

instance J.FromJSON Command where
  parseJSON (J.String s) = case T.unpack s of
    "run" -> return CRun ; "core" -> return CCore ; "asm" -> return CAsm ;
    _ -> fail "Invalid Command string"
  parseJSON val = J.prependFailure "parsing Command failed, " (J.typeMismatch "String" val)

instance J.ToJSON Command where
  toJSON cmd = J.String $ T.pack $ case cmd of
    CRun -> "run" ; CCore -> "core" ; CAsm -> "asm"
  toEncoding cmd = case cmd of
    CRun -> JE.string "run" ; CCore -> JE.string "core" ; CAsm -> JE.string "asm"

instance J.FromJSON Optimisation where
  parseJSON (J.String s) = case T.unpack s of
    "O0" -> return O0 ; "O1" -> return O1 ; "O2" -> return O2 ;
    _ -> fail "Invalid Optimisation string"
  parseJSON val = J.prependFailure "parsing Optimisation failed, " (J.typeMismatch "String" val)

instance J.ToJSON Optimisation where
  toJSON opt = J.String $ T.pack $ case opt of
    O0 -> "O0" ; O1 -> "O1" ; O2 -> "O2"
  toEncoding opt = case opt of
    O0 -> JE.string "O0" ; O1 -> JE.string "O1" ; O2 -> JE.string "O2"

instance J.FromJSON Version where
  parseJSON (J.String s) = return (Version (T.unpack s))
  parseJSON val = J.prependFailure "parsing Version failed, " (J.typeMismatch "String" val)

instance J.ToJSON Version where
  toJSON (Version s) = J.String (T.pack s)
  toEncoding (Version s) = JE.string s

instance J.FromJSON RunError where
  parseJSON (J.String s) = case T.unpack s of
    "timeout" -> return RETimeOut
    _ -> fail "Invalid RunError string"
  parseJSON val = J.prependFailure "parsing RunError failed, " (J.typeMismatch "String" val)

instance J.ToJSON RunError where
  toJSON RETimeOut = J.String (T.pack "timeout")
  toJSON REBackend = J.String (T.pack "backend")
  toEncoding RETimeOut = JE.string "timeout"
  toEncoding REBackend = JE.string "backend"

instance J.FromJSON a => J.FromJSON (Message a) where
  parseJSON (J.Object v) =
    Message <$> v J..: fromString "sig"
            <*> v J..: fromString "pkey"
            <*> v J..: fromString "con"
  parseJSON val = J.prependFailure "parsing Message failed, " (J.typeMismatch "Object" val)

instance J.ToJSON a => J.ToJSON (Message a) where
  toJSON (Message sig pkey con) =
    J.object [fromString "sig" J..= sig
             ,fromString "pkey" J..= pkey
             ,fromString "con" J..= con]
  toEncoding (Message sig pkey con) =
    JE.pairs (fromString "sig" J..= sig
           <> fromString "pkey" J..= pkey
           <> fromString "con" J..= con)

instance J.FromJSON RunRequest where
  parseJSON (J.Object v) =
    RunRequest <$> v J..: fromString "cmd"
               <*> v J..: fromString "src"
               <*> v J..: fromString "ver"
               <*> v J..: fromString "opt"
  parseJSON val = J.prependFailure "parsing RunRequest failed, " (J.typeMismatch "Object" val)

instance J.ToJSON RunRequest where
  toJSON (RunRequest cmd src ver opt) =
    J.object [fromString "cmd" J..= cmd
             ,fromString "src" J..= src
             ,fromString "ver" J..= ver
             ,fromString "opt" J..= opt]
  toEncoding (RunRequest cmd src ver opt) =
    JE.pairs (fromString "cmd" J..= cmd
           <> fromString "src" J..= src
           <> fromString "ver" J..= ver
           <> fromString "opt" J..= opt)

instance J.FromJSON RunResponse where
  parseJSON (J.Object v) =
    (RunResponseErr <$> v J..: fromString "err")
    <|> (RunResponseOk <$> (toExitCode <$> (v J..: fromString "ec"))
                       <*> (getJSONUTF8LBS <$> v J..: fromString "sout")
                       <*> (getJSONUTF8LBS <$> v J..: fromString "serr")
                       <*> v J..: fromString "timesecs")
    where toExitCode 0 = ExitSuccess
          toExitCode n = ExitFailure n
  parseJSON val = J.prependFailure "parsing RunResponse failed, " (J.typeMismatch "Object" val)

instance J.ToJSON RunResponse where
  toJSON (RunResponseErr err) = J.object [fromString "err" J..= err]
  toJSON (RunResponseOk ec sout serr timesecs) =
    J.object [fromString "ec" J..= (case ec of ExitSuccess -> 0 ; ExitFailure n -> n)
             ,fromString "sout" J..= JSONUTF8LBS sout
             ,fromString "serr" J..= JSONUTF8LBS serr
             ,fromString "timesecs" J..= timesecs]
  toEncoding (RunResponseErr err) = JE.pairs (fromString "err" J..= err)
  toEncoding (RunResponseOk ec sout serr timesecs) =
    JE.pairs (fromString "ec" J..= (case ec of ExitSuccess -> 0 ; ExitFailure n -> n)
           <> fromString "sout" J..= JSONUTF8LBS sout
           <> fromString "serr" J..= JSONUTF8LBS serr
           <> fromString "timesecs" J..= timesecs)

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

instance SigningBytes Lazy.ByteString where
  signingBytesB bs = BSB.word64LE (fromIntegral @Int64 @Word64 (Lazy.length bs)) <> BSB.lazyByteString bs

newtype SigningBytesUTF8String = SigningBytesUTF8String String

instance SigningBytes SigningBytesUTF8String where
  signingBytesB (SigningBytesUTF8String s) =
    BSB.word64LE (fromIntegral @Int @Word64 (length s)) <> BSB.stringUtf8 s

newtype SigningBytesUTF8Text = SigningBytesUTF8Text Text

instance SigningBytes SigningBytesUTF8Text where
  signingBytesB (SigningBytesUTF8Text s) =
    BSB.word64LE (fromIntegral @Int @Word64 (T.length s)) <> TE.encodeUtf8Builder s

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
  signingBytesB REBackend = BSB.word8 1

instance SigningBytes RunRequest where
  signingBytesB (RunRequest cmd src ver opt) =
    mconcat [signingBytesB cmd
            ,signingBytesB (SigningBytesUTF8Text src)
            ,signingBytesB ver
            ,signingBytesB opt]

instance SigningBytes RunResponse where
  signingBytesB (RunResponseErr err) =
    mconcat [BSB.word8 0
            ,signingBytesB err]
  signingBytesB (RunResponseOk ec sout serr timesecs) =
    mconcat [BSB.word8 1
            ,signingBytesB ec
            ,signingBytesB sout
            ,signingBytesB serr
            ,BSB.doubleLE timesecs]

instance SigningBytes VersionsResponse where
  signingBytesB (VersionsResponse l) =
    BSB.word64LE (fromIntegral (length l))
    <> mconcat (map (\(Version v) -> signingBytesB (SigningBytesUTF8String v)) l)

instance SigningBytes ExitCode where
  signingBytesB ExitSuccess = BSB.int64LE 0
  signingBytesB (ExitFailure ec) = BSB.int64LE (fromIntegral @Int @Int64 ec)
