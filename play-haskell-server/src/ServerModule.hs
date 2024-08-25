{-# LANGUAGE ExistentialQuantification #-}
module ServerModule where

import Control.Concurrent.STM (TVar, readTVarIO)
import Data.ByteString (ByteString)
import Snap.Core hiding (path)

import DB (Database)
import Pages (Pages)
import PlayHaskellTypes.Sign (SecretKey, PublicKey)
import Snap.Server.Utils


-- TODO: Perhaps this can be split out over modules as well
data Options = Options { oProxied :: Bool
                       , oPort :: Int
                       , oDBDir :: FilePath
                       , oSecKeyFile :: FilePath
                       , oAdminPassFile :: Maybe FilePath
                       , oPreloadFile :: Maybe FilePath }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options False 8123 "." "" Nothing Nothing

data GlobalContext = GlobalContext
  { gcDb :: Database
  , gcPagesVar :: TVar Pages
  , gcServerSecretKey :: SecretKey
  , gcAdminPassword :: Maybe ByteString
  , gcPreloadWorkers :: [(ByteString, PublicKey)]}

type MimeType = String

data StaticFile = StaticFile
  { sfPath :: FilePath       -- ^ Where to find the file
  , sfMount :: [ByteString]  -- ^ At what URI it should be accessible to the client
  , sfMime :: MimeType }     -- ^ Mime type of the file
  deriving (Show)

data ServerModule =
    forall ctx req. ServerModule
        { smMakeContext :: GlobalContext -> Options -> (ctx -> IO ()) -> IO ()  -- bracket
        , smParseRequest :: Method -> [ByteString] -> Maybe req
        , smHandleRequest :: GlobalContext -> ctx -> req -> Snap ()
        , smStaticFiles :: [StaticFile] }


staticFile :: String -> FilePath -> Snap ()
staticFile = staticFile' "static"

getPageFromGCtx :: (Pages -> a) -> GlobalContext -> IO a
getPageFromGCtx f gctx = f <$> readTVarIO (gcPagesVar gctx)
