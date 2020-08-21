module Main where

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
import Data.FileEmbed
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Word
import Snap.Core
import Snap.Http.Server
import System.IO
import System.IO.Unsafe
import System.Random

import qualified Embed as Embed
-- import qualified EmbedRuntime as Embed


findSubString :: String -> String -> Int
findSubString [] _ = error "findSubString: no such substring"
findSubString large small
  | take (length small) large == small = 0
  | otherwise = 1 + findSubString (tail large) small

htmlEscape :: ByteString -> Builder.Builder
htmlEscape bs
  | Just idx <- BS.elemIndex (fromIntegral (ord '<')) bs =
      mconcat [Builder.byteString (BS.take idx bs)
              ,Builder.byteString (Char8.pack "&lt;")
              ,htmlEscape (BS.drop (idx + 1) bs)]
  | otherwise = Builder.byteString bs

indexHTML :: ByteString
indexHTML = Embed.indexHTML

responseHTML :: (ByteString, ByteString, ByteString)
responseHTML =
    let marker1 = "[[[INSERT_KEY]]]"
        marker2 = "[[[INSERT_KEY2]]]"
        idx1 = findSubString fileContents marker1
        idx2 = idx1 + findSubString (drop idx1 fileContents) marker2
    in (Char8.pack (take idx1 fileContents)
       ,Char8.pack (drop (idx1 + length marker1) (take idx2 fileContents))
       ,Char8.pack (drop (idx2 + length marker2) fileContents))
  where
    fileContents = Char8.unpack Embed.responseHTML

pasteReadHTML :: (ByteString, ByteString, ByteString)
pasteReadHTML =
    let marker1 = "[[[INSERT_KEY]]]"
        marker2 = "[[[INSERT_CONTENTS]]]"
        idx1 = findSubString fileContents marker1
        idx2 = idx1 + findSubString (drop idx1 fileContents) marker2
    in (Char8.pack (take idx1 fileContents)
       ,Char8.pack (drop (idx1 + length marker1) (take idx2 fileContents))
       ,Char8.pack (drop (idx2 + length marker2) fileContents))
  where
    fileContents = Char8.unpack Embed.readHTML

maxUploadSize :: Word64
maxUploadSize = 32 * 1024 * 1024

alphabet :: ByteString
alphabet = Char8.pack (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

maxKeyLength :: Int
maxKeyLength = 8

type AtomicState = TVar State
type KeyType = Short.ShortByteString
type ContentsType = ByteString
data State = State (Map KeyType ContentsType) StdGen

genKey :: StdGen -> (KeyType, StdGen)
genKey gen =
    let (bs, gen') = genShortByteString maxKeyLength gen
        bs' = Short.toShort
                . BS.map (\x -> BS.index alphabet
                                    (fromIntegral x `rem` BS.length alphabet))
                . Short.fromShort
                $ bs
    in (bs', gen')

genKeySatisfying :: StdGen
                 -> (KeyType -> Bool)
                 -> (KeyType, StdGen)
genKeySatisfying gen predicate =
    let (key, gen') = genKey gen
    in if predicate key then (key, gen') else genKey gen'

storePaste :: IORef AtomicState -> ContentsType -> IO KeyType
storePaste stref contents = do
    var <- readIORef stref
    atomically $ do
        State mp gen <- readTVar var
        let (key, gen') = genKeySatisfying gen (`Map.notMember` mp)
            mp' = Map.insert key contents mp
        writeTVar var (State mp' gen')
        return key

getPaste :: IORef AtomicState -> KeyType -> IO (Maybe ContentsType)
getPaste stref key = do
    var <- readIORef stref
    atomically $ do
        State mp _ <- readTVar var
        return (Map.lookup key mp)

pasteResponse :: KeyType -> Lazy.ByteString
pasteResponse key =
    let (pre, mid, post) = responseHTML
    in Builder.toLazyByteString $ mconcat
        [Builder.byteString pre
        ,Builder.shortByteString key
        ,Builder.byteString mid
        ,Builder.shortByteString key
        ,Builder.byteString post]

pasteReadResponse :: KeyType -> ContentsType -> Lazy.ByteString
pasteReadResponse key contents =
    let (pre, mid, post) = pasteReadHTML
    in Builder.toLazyByteString $ mconcat
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

handleRequest :: IORef AtomicState -> Method -> ByteString -> Snap ()
handleRequest stref GET path
  | path == Char8.pack "/" = do
      writeBS indexHTML
  | Just key <- parsePasteGet path = do
      res <- liftIO $ getPaste stref key
      case res of
          Just contents -> writeLBS (pasteReadResponse key contents)
          Nothing -> error404 "Paste not found"
handleRequest stref POST path
  | path == Char8.pack "/paste" = do
      -- readRequestBody maxUploadSize
      mbody <- rqPostParam (Char8.pack "code") <$> getRequest
      case mbody of
          Just [body] -> do
              key <- liftIO $ storePaste stref body
              writeLBS (pasteResponse key)
          Nothing -> error400 "No paste given"
handleRequest _ _ _ = error404 "Page not found"

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
    stref <- newTVarIO (State mempty randgen) >>= newIORef
    httpServe config (server stref)
