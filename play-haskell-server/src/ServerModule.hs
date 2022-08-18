{-# LANGUAGE ExistentialQuantification #-}
module ServerModule where

import Control.Concurrent.STM (TVar, readTVarIO)
import Data.ByteString (ByteString)
import Snap.Core hiding (path)

import DB (Database)
import Pages (Pages)
import PlayHaskellTypes.Sign (SecretKey, PublicKey)
import Snap.Server.Utils
import Snap.Server.Utils.SpamDetect


-- TODO: Perhaps this can be split out over modules as well
data Options = Options { oProxied :: Bool
                       , oDBDir :: FilePath
                       , oSecKeyFile :: FilePath
                       , oAdminPassFile :: Maybe FilePath
                       , oPreloadFile :: Maybe FilePath }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options False "." "" Nothing Nothing

data SpamAction = PlayRunStart | PlayRunTimeoutFraction Double | PlaySave
  deriving (Show)

data GlobalContext = GlobalContext
  { gcSpam :: SpamDetect SpamAction ByteString
  , gcDb :: Database
  , gcPagesVar :: TVar Pages
  , gcServerSecretKey :: SecretKey
  , gcAdminPassword :: Maybe ByteString
  , gcPreloadWorkers :: [(ByteString, PublicKey)]}

type MimeType = String

data ServerModule =
    forall ctx req. ServerModule
        { smMakeContext :: GlobalContext -> Options -> (ctx -> IO ()) -> IO ()  -- bracket
        , smParseRequest :: Method -> [ByteString] -> Maybe req
        , smHandleRequest :: GlobalContext -> ctx -> req -> Snap ()
        , smStaticFiles :: [(FilePath, MimeType)] }


staticFile :: String -> FilePath -> Snap ()
staticFile = staticFile' "static"

getPageFromGCtx :: (Pages -> a) -> GlobalContext -> IO a
getPageFromGCtx f gctx = f <$> readTVarIO (gcPagesVar gctx)
