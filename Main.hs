{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Data.Time.Clock.POSIX (POSIXTime)
import Snap.Core hiding (path, method)
import Snap.Http.Server
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import qualified System.Posix.Signals as Signal
import System.Random

import qualified DB
import DB (Database, KeyType, ContentsType)
import SpamDetect
import Pages


alphabet :: ByteString
alphabet = Char8.pack (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

minKeyLength, maxKeyLength :: Int
(minKeyLength, maxKeyLength) = (8, 8)

maxPasteSize :: Int
maxPasteSize = 128 * 1024

genKey :: StdGen -> (KeyType, StdGen)
genKey gen =
    let (bs, gen') = genShortByteString maxKeyLength gen
        bs' = Short.toShort
                . BS.map (\x -> BS.index alphabet
                                    (fromIntegral x `rem` BS.length alphabet))
                . Short.fromShort
                $ bs
    in (bs', gen')

data Options = Options { oProxied :: Bool }

defaultOptions :: Options
defaultOptions = Options False

data Context = Context
    { cDB :: Database
    , cSpam :: SpamDetect ByteString }

data State = State
    { sRandGen :: StdGen
    , sPages :: Pages }

type AtomicState = TVar State

newState :: StdGen -> Pages -> State
newState randgen pages =
    State { sRandGen = randgen
          , sPages = pages }

genKey' :: AtomicState -> IO KeyType
genKey' var = atomically $ do
    state <- readTVar var
    let (key, gen') = genKey (sRandGen state)
    writeTVar var (state { sRandGen = gen' })
    return key

stateGetPage :: (Pages -> a) -> AtomicState -> IO a
stateGetPage f stvar =
    f . sPages <$> atomically (readTVar stvar)

-- returns the generated key, or an error string
genStorePaste :: Context -> AtomicState -> ContentsType -> IO (Either String KeyType)
genStorePaste context stvar contents =
    let loop iter = do
            key <- genKey' stvar
            DB.storePaste (cDB context) key contents >>= \case
                Nothing -> return (Right key)
                Just DB.ErrExists
                    | iter < 5 -> loop (iter + 1)  -- try again with a new key
                    | otherwise -> return (Left "Database full?")
                Just DB.ErrFull -> return (Left "Too many pastes submitted, please notify tomsmeding")
    in loop (0 :: Int)

getPaste :: Context -> KeyType -> IO (Maybe (Maybe POSIXTime, ContentsType))
getPaste context = DB.getPaste (cDB context)

pasteReadResponse :: AtomicState -> KeyType -> Maybe POSIXTime -> ContentsType -> IO ByteString
pasteReadResponse stvar key mdate files = do
    renderer <- stateGetPage pPasteRead stvar
    return $ renderer key mdate files

parsePasteGet :: ByteString -> Maybe KeyType
parsePasteGet bs = do
    guard (1 + minKeyLength <= BS.length bs && BS.length bs <= 1 + maxKeyLength)
    guard (BS.head bs == fromIntegral (ord '/'))
    let key = BS.drop 1 bs
    guard (fromIntegral (ord '/') `BS.notElem` key)
    return (Short.toShort key)

httpError :: Int -> String -> Snap ()
httpError code msg = do
    putResponse $ setResponseCode code emptyResponse
    writeBS (Char8.pack msg)

staticFile :: String -> FilePath -> Snap ()
staticFile mime path = do
    modifyResponse $
        setContentType (Char8.pack mime)
        . setHeader (fromString "Cache-Control") (Char8.pack "public max-age=3600")
    sendFile path

collectFilesFromPost :: Map ByteString [ByteString] -> ContentsType
collectFilesFromPost mp =
    let params = [(key, value) | (key, value:_) <- Map.assocs mp]
        codes = Map.fromList $ collectPrefixed (Char8.pack "code") params
        names = Map.fromList $ collectPrefixed (Char8.pack "name") params
        names' = Map.map (\bs -> if BS.null bs then Nothing else Just bs) names
    in Map.elems $ Map.intersectionWith (,) names' codes
  where
    collectPrefixed :: ByteString -> [(ByteString, a)] -> [(Int, a)]
    collectPrefixed prefix pairs =
        [(idx, value)
        | (key, value) <- pairs
        , BS.take 4 key == prefix
        , (idx, rest) <- maybeToList (Char8.readInt (BS.drop 4 key))
        , BS.null rest]

handleRequest :: Context -> AtomicState -> Method -> ByteString -> Snap ()
handleRequest context stvar GET path
  | path == Char8.pack "/" = do
      liftIO (stateGetPage pIndex stvar) >>= writeBS
  | path == Char8.pack "/highlight.pack.js" = staticFile "text/javascript" "highlight.pack.js"
  | path == Char8.pack "/highlight.pack.css" = staticFile "text/css" "highlight.pack.css"
  | BS.take 7 path == Char8.pack "/paste/" =
      redirect' (BS.drop 6 path) 301  -- moved permanently
  | Just key <- parsePasteGet path = do
      res <- liftIO $ getPaste context key
      case res of
          Just (mdate, contents) -> liftIO (pasteReadResponse stvar key mdate contents) >>= writeBS
          Nothing -> httpError 404 "Paste not found"
handleRequest context stvar POST path
  | path == Char8.pack "/paste" = do
      req <- getRequest
      let clientaddr = rqClientAddr req
      isSpam <- liftIO $ recordCheckSpam (cSpam context) clientaddr
      if isSpam
          then httpError 429 "Please slow down a bit, you're rate limited"
          else case collectFilesFromPost (rqPostParams req) of
                   [] -> httpError 400 "No paste given"
                   files
                     | sum (map (BS.length . snd) files) <= maxPasteSize -> do
                         mkey <- liftIO $ genStorePaste context stvar files
                         case mkey of
                             Right key -> redirect' (Char8.pack "/" `BS.append` Short.fromShort key) 303 -- see other
                             Left err -> httpError 500 err
                     | otherwise -> do
                         httpError 400 "Paste too large"
handleRequest _ _ _ _ = httpError 404 "Page not found"

installPageReloader :: AtomicState -> IO ()
installPageReloader stvar = do
    let handler = do
            pages <- pagesFromDisk
            atomically $ modifyTVar stvar $ \state -> state { sPages = pages }
            putStrLn "Reloaded pages"
    void $ Signal.installHandler Signal.sigUSR1 (Signal.Catch handler) Nothing

server :: Options -> Context -> AtomicState -> Snap ()
server options context stvar = do
    -- If we're proxied, set the source IP from the X-Forwarded-For header.
    when (oProxied options) ipHeaderFilter

    req <- getRequest
    let path = rqContextPath req `BS.append` rqPathInfo req
        method = rqMethod req
    handleRequest context stvar method path

config :: Config Snap a
config =
    let stderrlogger = ConfigIoLog (Char8.hPutStrLn stderr)
    in setAccessLog stderrlogger
       . setErrorLog stderrlogger
       . setPort 8123
       $ defaultConfig

main :: IO ()
main = do
    options <- getArgs >>= \case
        ["--proxied"] -> return (defaultOptions { oProxied = True })
        [] -> return defaultOptions
        _ -> do
            hPutStr stderr $ unlines $
                ["Usage:"
                ,"  ./pastebin-haskell [--proxied]"
                ,""
                ,"  --proxied   Assumes the server is running behind a proxy that sets"
                ,"              X-Forwarded-For instead of the source IP of a request"
                ,"              for rate limiting."]
            exitFailure

    DB.withDatabase $ \db -> do
        spam <- initSpamDetect
        let context = Context db spam

        -- Create state
        randgen <- newStdGen
        pages <- pagesFromDisk
        stvar <- newTVarIO (newState randgen pages)

        -- Reload pages from disk on SIGUSR1
        installPageReloader stvar

        -- Run server
        httpServe config (server options context stvar)
