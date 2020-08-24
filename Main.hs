module Main (main) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Char
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Snap.Core hiding (path, method)
import Snap.Http.Server
import System.IO
import qualified System.Posix.Signals as Signal
import System.Random

import Pages


htmlEscape :: ByteString -> Builder.Builder
htmlEscape bs
  | Just idx <- BS.elemIndex (fromIntegral (ord '<')) bs =
      mconcat [Builder.byteString (BS.take idx bs)
              ,Builder.byteString (Char8.pack "&lt;")
              ,htmlEscape (BS.drop (idx + 1) bs)]
  | otherwise = Builder.byteString bs

alphabet :: ByteString
alphabet = Char8.pack (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

maxKeyLength :: Int
maxKeyLength = 8

maxMemoryUsage :: Int
maxMemoryUsage = 128 * 1024 * 1024

genKey :: StdGen -> (KeyType, StdGen)
genKey gen =
    let (bs, gen') = genShortByteString maxKeyLength gen
        bs' = Short.toShort
                . BS.map (\x -> BS.index alphabet
                                    (fromIntegral x `rem` BS.length alphabet))
                . Short.fromShort
                $ bs
    in (bs', gen')

genKeySatisfying :: StdGen -> (KeyType -> Bool) -> (KeyType, StdGen)
genKeySatisfying gen predicate =
    let (key, gen') = genKey gen
    in if predicate key then (key, gen') else genKey gen'

type AtomicState = TVar State
type KeyType = Short.ShortByteString
type ContentsType = ByteString

data State = State
    { sPasteMap :: Map KeyType ContentsType
    , sHistory :: Seq KeyType
    , sRandGen :: StdGen
    , sTotalSize :: Int
    , sPages :: Pages }

newState :: StdGen -> Pages -> State
newState randgen pages =
    State { sPasteMap = mempty
          , sHistory = mempty
          , sRandGen = randgen
          , sTotalSize = 0
          , sPages = pages }

stateGetPage :: (Pages -> a) -> IORef AtomicState -> IO a
stateGetPage f stref =
    f . sPages <$> (readIORef stref >>= atomically . readTVar)

sizeOverheadPerPaste :: Int
sizeOverheadPerPaste =
    -- These are complete guesstimates!
    -- 3*keysize bytes overhead for history
    -- keysize bytes contents for history
    -- 3*keysize bytes overhead for pasteMap
    -- keysize bytes contents for pasteMap
    8 * maxKeyLength

purgeOldPastes :: State -> State
purgeOldPastes state
  | sTotalSize state + sizeOverheadPerPaste * Map.size (sPasteMap state)
        > maxMemoryUsage
  , old Seq.:<| rest <- sHistory state
  = let oldLength = maybe 0 BS.length (Map.lookup old (sPasteMap state))
    in purgeOldPastes $
        state { sPasteMap = Map.delete old (sPasteMap state)
              , sHistory = rest
              , sTotalSize = sTotalSize state - oldLength }
  | otherwise
  = state

storePaste :: IORef AtomicState -> ContentsType -> IO KeyType
storePaste stref contents = do
    var <- readIORef stref
    atomically $ do
        state@State { sPasteMap = mp } <- readTVar var
        let (key, gen') = genKeySatisfying (sRandGen state) (`Map.notMember` mp)
            mp' = Map.insert key contents mp
            state' = state { sPasteMap = mp'
                           , sRandGen = gen'
                           , sHistory = sHistory state Seq.|> key
                           , sTotalSize = sTotalSize state + BS.length contents }
            state'' = purgeOldPastes state'
        writeTVar var state''
        return key

getPaste :: IORef AtomicState -> KeyType -> IO (Maybe ContentsType)
getPaste stref key = do
    var <- readIORef stref
    atomically $ Map.lookup key . sPasteMap <$> readTVar var

diagnostics :: IORef AtomicState -> IO ()
diagnostics stref = do
    state <- readIORef stref >>= (atomically . readTVar)
    putStrLn $
        "diagnostics: "
            ++ show (Map.size (sPasteMap state)) ++ " pastes, "
            ++ show (sTotalSize state + Map.size (sPasteMap state) * sizeOverheadPerPaste) ++ " memory used"
            -- ++ ": " ++ intercalate " " (map (Char8.unpack . Short.fromShort)
            --                                 (toList (sHistory state)))
    return ()

pasteReadResponse :: IORef AtomicState -> KeyType -> ContentsType -> IO Lazy.ByteString
pasteReadResponse stref key contents = do
    (pre, mid, post) <- stateGetPage pPasteRead stref
    return $ Builder.toLazyByteString $ mconcat
        [Builder.byteString pre
        ,Builder.shortByteString key
        ,Builder.byteString mid
        ,htmlEscape contents
        ,Builder.byteString post]

parsePasteGet :: ByteString -> Maybe KeyType
parsePasteGet bs = do
    guard (BS.length bs <= 7 + maxKeyLength)
    guard (BS.take 7 bs == Char8.pack "/paste/")
    let key = BS.drop 7 bs
    guard (fromIntegral (ord '/') `BS.notElem` key)
    return (Short.toShort key)

error400 :: String -> Snap ()
error400 msg = do
    putResponse $ setResponseCode 400 emptyResponse
    writeBS (Char8.pack msg)

error404 :: String -> Snap ()
error404 msg = do
    putResponse $ setResponseCode 404 emptyResponse
    writeBS (Char8.pack msg)

staticFile :: String -> FilePath -> Snap ()
staticFile mime path = do
    modifyResponse $ setContentType (Char8.pack mime)
    sendFile path

handleRequest :: IORef AtomicState -> Method -> ByteString -> Snap ()
handleRequest stref GET path
  | path == Char8.pack "/" = do
      liftIO (stateGetPage pIndex stref) >>= writeBS
  | path == Char8.pack "/highlight.pack.js" = staticFile "text/javascript" "highlight.pack.js"
  | path == Char8.pack "/highlight.pack.css" = staticFile "text/css" "highlight.pack.css"
  | Just key <- parsePasteGet path = do
      res <- liftIO $ getPaste stref key
      case res of
          Just contents -> liftIO (pasteReadResponse stref key contents) >>= writeLBS
          Nothing -> error404 "Paste not found"
handleRequest stref POST path
  | path == Char8.pack "/paste" = do
      mbody <- rqPostParam (Char8.pack "code") <$> getRequest
      case mbody of
          Just [body] -> do
              key <- liftIO $ storePaste stref body
              liftIO $ diagnostics stref
              redirect' (Char8.pack "/paste/" `BS.append` Short.fromShort key) 303
          Just _ -> error400 "Multiple code parameters given"
          Nothing -> error400 "No paste given"
handleRequest _ _ _ = error404 "Page not found"

installPageReloader :: IORef AtomicState -> IO ()
installPageReloader stref = do
    let handler = do
            pages <- pagesFromDisk
            var <- readIORef stref
            atomically $ modifyTVar var $ \state -> state { sPages = pages }
            putStrLn "Reloaded pages"
    void $ Signal.installHandler Signal.sigUSR1 (Signal.Catch handler) Nothing

server :: IORef AtomicState -> Snap ()
server stref = do
    req <- getRequest
    let path = rqContextPath req `BS.append` rqPathInfo req
        method = rqMethod req
    handleRequest stref method path

config :: Config Snap a
config =
    let stderrlogger = ConfigIoLog (Char8.hPutStrLn stderr)
    in setAccessLog stderrlogger
       . setErrorLog stderrlogger
       . setPort 8123
       $ defaultConfig

main :: IO ()
main = do
    randgen <- newStdGen
    pages <- pagesFromDisk
    stref <- newTVarIO (newState randgen pages) >>= newIORef
    installPageReloader stref
    httpServe config (server stref)
