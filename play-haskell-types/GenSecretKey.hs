{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Foreign.Marshal.Alloc (allocaBytes)
import System.Environment
import System.Exit
import System.IO
import System.Posix.Files (setFileMode, ownerReadMode, ownerWriteMode)

import Snap.Server.Utils.Hex
import qualified PlayHaskellTypes.Sign as Sign


helpString :: String
helpString =
  "Usage: gen-secret-key [outfile]\n\
  \If an output file is given, the key is written to the specified file. Otherwise,\n\
  \the key is written to standard output.\n\
  \The public key corresponding to the generated secret key is written to standard\n\
  \error."

main :: IO ()
main = do
  moutfname <- getArgs >>= \case
    "-h" : _ -> putStrLn helpString >> exitSuccess
    "--help" : _ -> putStrLn helpString >> exitSuccess
    [outfname] -> return (Just outfname)
    [] -> return Nothing
    _ -> putStrLn helpString >> exitFailure

  mskeybytes <- withBinaryFile "/dev/urandom" ReadMode $ \fh -> do
    allocaBytes 32 $ \ptr -> do
      nread <- hGetBuf fh ptr 32
      if nread == 32
        then Just <$> BS.packCStringLen (ptr, 32)
        else return Nothing

  case mskeybytes of
    Nothing -> die "Reading from /dev/urandom failed"
    Just skeybytes -> do
      let hexstring = hexEncode (BSS.toShort skeybytes)
      case moutfname of
        Just outfname -> do
          writeFile outfname hexstring
          setFileMode outfname (ownerReadMode .|. ownerWriteMode)
        Nothing -> putStrLn hexstring

      case Sign.publicKey <$> Sign.readSecretKey skeybytes of
        Nothing -> die "Secret key construction failed?"
        Just (Sign.PublicKey pkey) -> hPutStrLn stderr ("Public key: " ++ hexEncode pkey)
