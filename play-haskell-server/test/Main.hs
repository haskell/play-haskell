{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.List (tails)
import Network.HTTP
import Text.JSON.Shim
import qualified System.Clock as Clock
import Data.Maybe (catMaybes)
import System.Exit


duration :: IO a -> IO (Double, a)
duration action = do
  starttm <- Clock.getTime Clock.Monotonic
  res <- action
  endtm <- Clock.getTime Clock.Monotonic
  let diff = Clock.diffTimeSpec starttm endtm
      secs = fromIntegral (Clock.sec diff) + fromIntegral (Clock.nsec diff) / 1e9
  return (secs, res)

infixOf :: Eq a => [a] -> [a] -> Bool
infixOf small large = any (\l -> and (zipWith (==) small l)) (tails large)


data Command = CRun | CCore | CAsm
  deriving (Show)
commandString :: Command -> String
commandString = \case CRun -> "run" ; CCore -> "core" ; CAsm -> "asm"
newtype Source = Source String
  deriving (Show)
newtype Version = Version String
  deriving (Show)
newtype Opt = Opt String
  deriving (Show)

data RunRes = RunRes Int (Maybe (String, String))
  deriving (Show)

data Error = Ratelimit | Busy
  deriving (Show)

getChallenge :: IO String
getChallenge = do
  simpleHTTP (getRequest "http://localhost:8123/play/challenge") >>= \case
    Left err -> error $ "Connection error: " ++ show err
    Right resp
      | rspCode resp == (2,0,0) ->
          return (rspBody resp)
      | otherwise ->
          error $ "Uncaught status code " ++ show (rspCode resp) ++ "\n\
                  \With body: " ++ show (rspBody resp)

runRequest :: String -> Command -> Source -> Version -> Opt -> IO (Either Error RunRes)
runRequest challenge cmd (Source source) (Version version) (Opt opt) = do
  let req = postRequestWithBody
              ("http://localhost:8123/play/" ++ commandString cmd)
              "text/json"
              (encodeJSON
                 (jsObject [("challenge", jsString challenge)
                           ,("source", jsString source)
                           ,("version", jsString version)
                           ,("opt", jsString opt)]))
  simpleHTTP req >>= \case
    Left err -> error $ "Connection error: " ++ show err
    Right resp
      | rspCode resp == (2,0,0)
      , Just (JSObject (fromJSObject -> assocs)) <- decodeJSON (rspBody resp)
      , Just (JSRational _ (round -> ec)) <- lookup "ec" assocs
      -> case (lookup "sout" assocs, lookup "serr" assocs) of
           (Just (JSString out), Just (JSString err)) ->
             return (Right (RunRes ec (Just (fromJSString out, fromJSString err))))
           _ ->
             return (Right (RunRes ec Nothing))
      | rspCode resp == (4,2,9)
      -> return (Left Ratelimit)
      | rspCode resp == (5,0,3)
      , "busy" `infixOf` rspBody resp
      -> return (Left Busy)
      | otherwise
      -> error $ "Uncaught status code " ++ show (rspCode resp) ++ "\n\
                 \With body: " ++ show (rspBody resp)

testParallelism :: String -> IO Bool
testParallelism challenge = do
  numPar <- getNumCapabilities
  chan <- newChan
  forM_ [1..numPar] $ \i -> forkIO $ do
    threadDelay (100000 * i)
    putStrLn $ "Sending request " ++ show i ++ "..."
    (dur, res) <- duration $ runRequest challenge CRun (Source program) (Version "8.10.7") (Opt "O1")
    case res of
      Right (RunRes 0 (Just ("", ""))) -> do
        writeChan chan (Just dur)
      Right (RunRes 0 (Just (out, err))) -> do
        putStrLn $ "Wrong output: " ++ show out ++ "; " ++ show err
        writeChan chan Nothing
      Right (RunRes 0 Nothing) -> do
        putStrLn $ "Output/error missing"
        writeChan chan Nothing
      Right (RunRes ec _) -> do
        putStrLn $ "Exit code not zero: " ++ show ec
        writeChan chan Nothing
      Left Ratelimit -> do
        putStrLn $ "Ratelimit"
        writeChan chan Nothing
      Left Busy -> do
        putStrLn $ "Busy"
        writeChan chan Nothing
  durs <- catMaybes <$> replicateM numPar (readChan chan)
  return $ all (\d -> 2.5 < d && d < 4) durs
  where
    program :: String
    program = "import Control.Concurrent\n\
              \main :: IO ()\n\
              \main = threadDelay 3000000\n"

runTestExit :: IO Bool -> IO ()
runTestExit act = act >>= \case True -> return ()
                                False -> exitFailure

main :: IO ()
main = do
  challenge <- getChallenge
  runTestExit (testParallelism challenge)
