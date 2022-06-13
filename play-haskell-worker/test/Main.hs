{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP
import System.Environment
import System.Exit

import PlayHaskellTypes
import PlayHaskellTypes.Sign (SecretKey)
import qualified PlayHaskellTypes.Sign as Sign
import Snap.Server.Utils.Hex


runRequest :: SecretKey -> Command -> Text -> Version -> Optimisation -> IO (Message RunResponse)
runRequest skey cmd source version opt = do
  let msg = signMessage skey
              RunRequest { runreqCommand = cmd
                         , runreqSource = source
                         , runreqVersion = version
                         , runreqOpt = opt }
  print msg
  let req = postRequestWithBody
              ("http://localhost:8124/job")
              "text/json"
              (UTF8.toString (J.encode msg))
  simpleHTTP req >>= \case
    Left err -> error $ "Connection error: " ++ show err
    Right resp
      | rspCode resp == (2,0,0)
      -> case J.decode' (UTF8.fromString (rspBody resp)) of
           Just rr -> return rr
           Nothing -> error $ "JSON decode error: " ++ rspBody resp
      | otherwise
      -> error $ "Uncaught status code " ++ show (rspCode resp) ++ "\n\
                 \With body: " ++ show (rspBody resp)

main :: IO ()
main = do
  (skeyfname, workerPkeystr) <- getArgs >>= \case
    [skeyfname, workerPkeystr] -> return (skeyfname, workerPkeystr)
    _ -> die "Usage: worker-test <server-secret-key.txt> <worker public key>"
  skeybytes <- hexDecodeBS <$> readFile skeyfname >>= \case
    Just skeybytes -> return skeybytes
    Nothing -> die "Hex decode of secret key failed"
  skey <- case Sign.readSecretKey skeybytes of
    Just skey -> return skey
    Nothing -> die "Invalid secret key in file"
  workerPkey <- case hexDecodeBS workerPkeystr >>= Sign.readPublicKey of
                  Just workerPkey -> return workerPkey
                  Nothing -> die "Hex decode of public key failed"

  response <- runRequest skey CRun (T.pack program) (Version "8.10.7") O1
  print response
  when (sesmsgPublicKey response /= workerPkey) $
    die $ "Response public key unequal to worker public key!\n\
          \Worker   pkey: " ++ show workerPkey ++ "\n\
          \Response pkey: " ++ show (sesmsgPublicKey response)
  when (not $ Sign.verify workerPkey (signingBytes (sesmsgContent response)) (sesmsgSignature response)) $
    die "Invalid signature in response!"
  putStrLn "ok!"
  where
    program :: String
    program = "import Control.Concurrent\n\
              \main :: IO ()\n\
              \main = threadDelay 500000 >> print 42\n"
