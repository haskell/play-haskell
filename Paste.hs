{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Paste (pasteModule) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList, isNothing)
import qualified Data.Text.Encoding as Enc
import Data.Time.Clock (nominalDay)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Snap.Core hiding (path, method)
import System.Random
import Text.Read (readMaybe)

import Paste.Archive
import qualified Paste.DB as DB
import Paste.DB (Database, ClientAddr, KeyType, Contents(..))
import Paste.HighlightCSS
import Paste.Pages
import ServerModule
import SpamDetect hiding (Action(..))
import qualified SpamDetect as Spam (Action(..))


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

writeHTML :: MonadSnap m => ByteString -> m ()
writeHTML bs = do
    modifyResponse $ setContentType "text/html; charset=utf-8"
    writeBS bs

data Context = Context
    { cHighlightCSS :: ByteString }

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
    f . sPages <$> readTVarIO stvar

-- returns the generated key, or an error string
genStorePaste :: GlobalContext -> AtomicState -> ClientAddr -> Contents -> IO (Either String KeyType)
genStorePaste gctx stvar srcip contents =
    let loop iter = do
            key <- genKey' stvar
            DB.storePaste (gcDb gctx) srcip key contents >>= \case
                Nothing -> return (Right key)
                Just DB.ErrExists
                    | iter < 5 -> loop (iter + 1)  -- try again with a new key
                    | otherwise -> return (Left "Database full?")
                Just DB.ErrFull -> return (Left "Too many pastes submitted, please notify tomsmeding")
    in loop (0 :: Int)

getPaste :: GlobalContext -> KeyType -> IO (Maybe (Maybe POSIXTime, Contents))
getPaste gctx = DB.getPaste (gcDb gctx)

indexResponse :: AtomicState -> Contents -> IO ByteString
indexResponse stvar contents = do
    renderer <- stateGetPage pIndex stvar
    return $ renderer contents

pasteReadResponse :: AtomicState -> KeyType -> Maybe POSIXTime -> Contents -> IO ByteString
pasteReadResponse stvar key mdate contents = do
    renderer <- stateGetPage pPasteRead stvar
    now <- getPOSIXTime
    return $ renderer now key mdate contents

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

data WhatRequest
    = GetIndex
    | ReadPaste ByteString
    | ReadPasteRaw ByteString Int
    | ReadPasteOld ByteString
    | EditPaste ByteString
    | HighlightCSS
    | StorePaste
    | DownloadPaste ByteString

parseRequest :: Method -> [ByteString] -> Maybe WhatRequest
parseRequest method comps =
    case (method, comps) of
        (GET, []) -> Just GetIndex
        (GET, [x]) | canBeKey x -> Just (ReadPaste x)
        (GET, [x, "edit"]) | canBeKey x -> Just (EditPaste x)
        (GET, [x, "raw"]) | canBeKey x -> Just (ReadPasteRaw x 1)
        (GET, [x, "raw", y]) | canBeKey x, Just idx <- readMaybe (Char8.unpack y) -> Just (ReadPasteRaw x idx)
        (GET, [x, "download"]) | canBeKey x -> Just (DownloadPaste x)
        (GET, ["paste", x]) | canBeKey x -> Just (ReadPasteOld x)
        (GET, ["highlight.pack.css"]) -> Just HighlightCSS
        (POST, ["paste"]) -> Just StorePaste
        _ -> Nothing
  where
    canBeKey :: ByteString -> Bool
    canBeKey key = minKeyLength <= BS.length key && BS.length key <= maxKeyLength

handleRequest :: GlobalContext -> Context -> AtomicState -> WhatRequest -> Snap ()
handleRequest gctx context stvar = \case
    GetIndex -> liftIO (indexResponse stvar (Contents [] Nothing Nothing)) >>= writeHTML
    EditPaste key -> do
        liftIO (getPaste gctx key) >>= \case
            Just (_, Contents files _ _) ->
                -- Replace parent (if any) with the edited paste
                liftIO (indexResponse stvar (Contents files (Just key) Nothing)) >>= writeHTML
            Nothing -> httpError 404 "Paste not found"
    ReadPaste key -> do
        liftIO (getPaste gctx key) >>= \case
            Just (mdate, contents) -> liftIO (pasteReadResponse stvar key mdate contents) >>= writeHTML
            Nothing -> httpError 404 "Paste not found"
    ReadPasteRaw key idx -> do
        liftIO (getPaste gctx key) >>= \case
            Just (_, Contents files _ _)
              | 1 <= idx, idx <= length files -> do
                  modifyResponse $ setContentType "text/plain; charset=utf-8"
                  writeBS (snd (files !! (idx - 1)))
              | otherwise -> httpError 404 "File index out of range for paste"
            Nothing -> httpError 404 "Paste not found"
    ReadPasteOld name -> redirect' (Char8.cons '/' name) 301  -- moved permanently
    StorePaste -> do
        req <- getRequest
        isSpam <- liftIO $ recordCheckSpam Spam.Post (gcSpam gctx) (rqClientAddr req)
        now <- liftIO getPOSIXTime
        if isSpam
            then httpError 429 "Please slow down a bit, you're rate limited"
            else handleNonSpamSubmit (collectContentsFromPost now (rqPostParams req))
    HighlightCSS -> do
        modifyResponse (applyStaticFileHeaders "text/css")
        writeBS (cHighlightCSS context)
    DownloadPaste key -> do
        liftIO (getPaste gctx key) >>= \case
            Just (mdate, Contents files _ _) -> do
                let disposition = BS.concat ["attachment; filename=\"", key, ".tar.gz\""]
                modifyResponse $
                    setContentType "application/gzip"
                    . addHeader "Content-Disposition" disposition
                writeLBS (createArchive key mdate files)
            Nothing -> httpError 404 "Paste not found"
  where
    handleNonSpamSubmit :: Contents -> Snap ()
    handleNonSpamSubmit (Contents [] _ _) = httpError 400 "No paste given"
    handleNonSpamSubmit contents@(Contents files _ _)
      | and [isNothing m && BS.null c | (m, c) <- files] =
          httpError 400 "No paste given"
      | or [isLeft (Enc.decodeUtf8' c) | (_, c) <- files] =
          httpError 400 "Invalid encoding; paste must be UTF-8"
      | sum (map (BS.length . snd) files) <= maxPasteSize = do
          req <- getRequest
          mkey <- liftIO $ genStorePaste gctx stvar (Char8.unpack (rqClientAddr req)) contents
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

refreshPages :: AtomicState -> IO ()
refreshPages stvar = do
    pages <- pagesFromDisk
    atomically $ modifyTVar stvar $ \state -> state { sPages = pages }
    putStrLn "Reloaded pages"

startExpiredRemoveService :: Database -> IO ()
startExpiredRemoveService db = void $ forkIO $ forever $ do
    threadDelay (6 * 3600 * 1000000)  -- 6 hours
    DB.removeExpiredPastes db

pasteModule :: ServerModule
pasteModule = ServerModule
    { smMakeContext = \gctx _options cont -> do
          css <- processHighlightCSS
          let context = Context css

          -- Create state
          randgen <- newStdGen
          pages <- pagesFromDisk
          stvar <- newTVarIO (newState randgen pages)

          -- Start services
          startExpiredRemoveService (gcDb gctx)

          -- Run server
          cont (context, stvar)
    , smParseRequest = parseRequest
    , smHandleRequest = \gctx (ctx, stvar) -> handleRequest gctx ctx stvar
    , smStaticFiles =
        [("highlight.pack.js", "text/javascript")
        -- ,("highlight.pack.css", "text/css")  -- this one is generated, not a static file
        ,("robots.txt", "text/plain")]
    , smReloadPages = \(_, stvar) -> refreshPages stvar }
