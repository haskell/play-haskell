{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP
import System.Environment
import System.Exit

import PlayHaskellTypes
import PlayHaskellTypes.Sign (SecretKey, PublicKey)
import qualified PlayHaskellTypes.Sign as Sign
import Snap.Server.Utils.Hex


runRequest :: SecretKey -> Command -> Text -> Version -> Optimisation -> IO (Message RunResponse)
runRequest skey cmd source version opt = do
  let msg = signMessage skey
              RunRequest { runreqCommand = cmd
                         , runreqSource = source
                         , runreqVersion = version
                         , runreqOpt = opt }
  -- print msg
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

singleTestRequest :: SecretKey -> PublicKey -> IO (Message RunResponse)
singleTestRequest skey workerPkey = do
  response <- runRequest skey CRun (T.pack program) (Version "8.10.7") O1
  when (sesmsgPublicKey response /= workerPkey) $ do
    print response
    die $ "Response public key unequal to worker public key!\n\
          \Worker   pkey: " ++ show workerPkey ++ "\n\
          \Response pkey: " ++ show (sesmsgPublicKey response)
  when (not $ Sign.verify workerPkey (signingBytes (sesmsgContent response)) (sesmsgSignature response)) $ do
    print response
    die "Invalid signature in response!"
  return response
  where
    program :: String
    program = "import Control.Concurrent\n\
              \main :: IO ()\n\
              \main = threadDelay 500000 >> print 42\n"

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

  -- Try a single request
  _ <- singleTestRequest skey workerPkey

  -- Try some parallel requests
  let nthreads = 10 :: Int
  chan <- newChan
  forM_ [1..nthreads] $ \i -> forkIO $ do
    putStrLn $ "Firing " ++ show i
    _ <- singleTestRequest skey workerPkey
    putStrLn $ "Done " ++ show i
    writeChan chan ()
  forM_ [1..nthreads] $ \i -> do
    readChan chan
    putStrLn $ "Joined " ++ show i

  putStrLn "ok!"
