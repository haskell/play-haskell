{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.List as List (lookup)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList, isNothing)
import Data.String (fromString)
import Data.Time.Clock (nominalDay)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Snap.Core hiding (path, method)
import Snap.Http.Server
import System.IO
import qualified System.Posix.Signals as Signal
import System.Random
import Text.Read (readMaybe)

import Archive
import qualified DB
import DB (Database, ClientAddr, KeyType, Contents(..))
import qualified Options as Opt
import SpamDetect hiding (Action(..))
import qualified SpamDetect as Spam (Action(..))
import Pages


alphabet :: ByteString
alphabet = Char8.pack (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

minKeyLength, maxKeyLength :: Int
(minKeyLength, maxKeyLength) = (8, 8)

maxPasteSize :: Int
maxPasteSize = 128 * 1024

genKey :: StdGen -> (KeyType, StdGen)
genKey gen =
    let (bs, gen') = genByteString maxKeyLength gen
        intoAlphabet n = BS.index alphabet (fromIntegral n `rem` BS.length alphabet)
    in (BS.map intoAlphabet bs, gen')

data Options = Options { oProxied :: Bool
                       , oDBDir :: FilePath }

defaultOptions :: Options
defaultOptions = Options False "."

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
genStorePaste :: Context -> AtomicState -> ClientAddr -> Contents -> IO (Either String KeyType)
genStorePaste context stvar srcip contents =
    let loop iter = do
            key <- genKey' stvar
            DB.storePaste (cDB context) srcip key contents >>= \case
                Nothing -> return (Right key)
                Just DB.ErrExists
                    | iter < 5 -> loop (iter + 1)  -- try again with a new key
                    | otherwise -> return (Left "Database full?")
                Just DB.ErrFull -> return (Left "Too many pastes submitted, please notify tomsmeding")
    in loop (0 :: Int)

getPaste :: Context -> KeyType -> IO (Maybe (Maybe POSIXTime, Contents))
getPaste context = DB.getPaste (cDB context)

indexResponse :: AtomicState -> Contents -> IO ByteString
indexResponse stvar contents = do
    renderer <- stateGetPage pIndex stvar
    return $ renderer contents

pasteReadResponse :: AtomicState -> KeyType -> Maybe POSIXTime -> Contents -> IO ByteString
pasteReadResponse stvar key mdate contents = do
    renderer <- stateGetPage pPasteRead stvar
    now <- getPOSIXTime
    return $ renderer now key mdate contents

httpError :: Int -> String -> Snap ()
httpError code msg = do
    putResponse $ setResponseCode code emptyResponse
    writeBS (Char8.pack msg)

staticFile :: String -> FilePath -> Snap ()
staticFile mime path = do
    modifyResponse $
        setContentType (Char8.pack mime)
        . setHeader (fromString "Cache-Control") "public max-age=3600"
    sendFile path

collectContentsFromPost :: POSIXTime -> Map ByteString [ByteString] -> Contents
collectContentsFromPost now mp =
    let params = [(key, value) | (key, value:_) <- Map.assocs mp]
        codes = Map.fromList (collectPrefixed "code" params)
        names = Map.fromList (collectPrefixed "name" params)
        names' = Map.map (\bs -> if BS.null bs then Nothing else Just bs) names
        files = Map.elems $ Map.intersectionWith (,) names' codes
        mparent = case Map.lookup "parent" mp of
                    Just (parent : _) -> Just parent
                    _ -> Nothing
        mexpire = case Map.lookup "expire" mp of
                    Just (expire : _) -> case expire of
                        "never" -> Nothing
                        "day" -> Just (now + nominalDay)
                        "week" -> Just (now + 7 * nominalDay)
                        "month" -> Just (now + 31 * nominalDay)
                        "year" -> Just (now + 366 * nominalDay)
                        _ -> Nothing  -- ¯\_(ツ)_/¯
                    _ -> Nothing
    in Contents files mparent mexpire
  where
    collectPrefixed :: ByteString -> [(ByteString, a)] -> [(Int, a)]
    collectPrefixed prefix pairs =
        [(idx, value)
        | (key, value) <- pairs
        , BS.take 4 key == prefix
        , (idx, rest) <- maybeToList (Char8.readInt (BS.drop 4 key))
        , BS.null rest]

staticFiles :: [(ByteString, (FilePath, String))]
staticFiles =
    [(Char8.pack path, (path, mime))
    | (path, mime) <-
        [("highlight.pack.js", "text/javascript")
        ,("highlight.pack.css", "text/css")
        ,("robots.txt", "text/plain")]]

data WhatRequest
    = GetIndex
    | ReadPaste ByteString
    | ReadPasteRaw ByteString Int
    | ReadPasteOld ByteString
    | EditPaste ByteString
    | StaticFile String FilePath
    | StorePaste
    | DownloadPaste ByteString

parseRequest :: Method -> ByteString -> Maybe WhatRequest
parseRequest _ path
  | BS.null path || BS.head path /= fromIntegral (ord '/')
      = Nothing
parseRequest method path =
    let comps = BS.split (fromIntegral (ord '/')) (trimSlashes path)
    in case (method, comps) of
           (GET, []) -> Just GetIndex
           (GET, [x]) | canBeKey x -> Just (ReadPaste x)
           (GET, [x, "edit"]) | canBeKey x -> Just (EditPaste x)
           (GET, [x, "raw"]) | canBeKey x -> Just (ReadPasteRaw x 1)
           (GET, [x, "raw", y]) | canBeKey x, Just idx <- readMaybe (Char8.unpack y) -> Just (ReadPasteRaw x idx)
           (GET, [x, "download"]) | canBeKey x -> Just (DownloadPaste x)
           (GET, ["paste", x]) | canBeKey x -> Just (ReadPasteOld x)
           (GET, [x]) | Just (path', mime) <- List.lookup x staticFiles -> Just (StaticFile mime path')
           (POST, ["paste"]) -> Just StorePaste
           _ -> Nothing
  where
    canBeKey :: ByteString -> Bool
    canBeKey key = minKeyLength <= BS.length key && BS.length key <= maxKeyLength

    trimSlashes :: ByteString -> ByteString
    trimSlashes = let slash = fromIntegral (ord '/')
                  in BS.dropWhile (== slash) . BS.dropWhileEnd (== slash)

handleRequest :: Context -> AtomicState -> WhatRequest -> Snap ()
handleRequest context stvar = \case
    GetIndex -> liftIO (indexResponse stvar (Contents [] Nothing Nothing)) >>= writeBS
    EditPaste key -> do
        liftIO (getPaste context key) >>= \case
            Just (_, Contents files _ _) ->
                -- Replace parent (if any) with the edited paste
                liftIO (indexResponse stvar (Contents files (Just key) Nothing)) >>= writeBS
            Nothing -> httpError 404 "Paste not found"
    ReadPaste key -> do
        liftIO (getPaste context key) >>= \case
            Just (mdate, contents) -> liftIO (pasteReadResponse stvar key mdate contents) >>= writeBS
            Nothing -> httpError 404 "Paste not found"
    ReadPasteRaw key idx -> do
        liftIO (getPaste context key) >>= \case
            Just (_, Contents files _ _)
              | 1 <= idx, idx <= length files -> writeBS (snd (files !! (idx - 1)))
              | otherwise -> httpError 404 "File index out of range for paste"
            Nothing -> httpError 404 "Paste not found"
    ReadPasteOld name -> redirect' (Char8.cons '/' name) 301  -- moved permanently
    StorePaste -> do
        req <- getRequest
        let clientaddr = rqClientAddr req
        isSpam <- liftIO $ recordCheckSpam Spam.Post (cSpam context) clientaddr
        now <- liftIO getPOSIXTime
        if isSpam
            then httpError 429 "Please slow down a bit, you're rate limited"
            else handleNonSpamSubmit (collectContentsFromPost now (rqPostParams req))
    StaticFile mime path -> staticFile mime path
    DownloadPaste key -> do
        liftIO (getPaste context key) >>= \case
            Just (_, Contents files _ _) -> do
                let disposition = BS.concat ["attachment; filename=\"", key, ".tar.gz\""]
                modifyResponse $
                    setContentType "application/gzip"
                    . addHeader "Content-Disposition" disposition
                writeLBS (Archive.createArchive key files)
            Nothing -> httpError 404 "Paste not found"
  where
    handleNonSpamSubmit :: Contents -> Snap ()
    handleNonSpamSubmit (Contents [] _ _) = httpError 400 "No paste given"
    handleNonSpamSubmit contents@(Contents files _ _)
      | all id [isNothing m && BS.null c | (m, c) <- files] =
          httpError 400 "No paste given"
      | sum (map (BS.length . snd) files) <= maxPasteSize = do
          req <- getRequest
          mkey <- liftIO $ genStorePaste context stvar (Char8.unpack (rqClientAddr req)) contents
          case mkey of
              Right key -> do
                  let suffix = "/" `BS.append` key
                  -- Perform a manual redirect because snap's redirect combinator early-exits
                  modifyResponse $
                      setResponseStatus 303 "See other"
                      . setHeader "Location" suffix
                  writeBS ("https://" `BS.append` rqHostName req `BS.append` suffix)
              Left err -> httpError 500 err
      | otherwise = do
          httpError 400 "Paste too large"

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
    case parseRequest method path of
        Just what -> handleRequest context stvar what
        Nothing -> httpError 404 "Page not found"

config :: Config Snap a
config =
    let stderrlogger = ConfigIoLog (Char8.hPutStrLn stderr)
    in setAccessLog stderrlogger
       . setErrorLog stderrlogger
       . setPort 8123
       $ defaultConfig

main :: IO ()
main = do
    options <- Opt.parseOptions $ Opt.Interface defaultOptions $ Map.fromList
        [("--proxied", Opt.Flag "Assumes the server is running behind a proxy that sets \
                                \X-Forwarded-For, instead of using the source IP of a \
                                \request for rate limiting."
                                (\o -> o { oProxied = True }))
        ,("--dbdir", Opt.Setter "Sets directory to store pastes.db in."
                                (\o s -> o { oDBDir = s }))
        ,("--help", Opt.Help)
        ,("-h", Opt.Help)]

    DB.withDatabase (oDBDir options) $ \db -> do
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
