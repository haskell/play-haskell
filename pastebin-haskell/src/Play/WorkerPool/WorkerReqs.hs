module Play.WorkerPool.WorkerReqs where

import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import Data.String (fromString)
import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Client.TLS as N

import PlayHaskellTypes


data Addr = Addr ByteString
  deriving (Show)

getVersions :: N.Manager -> Addr -> IO [Version]
getVersions mgr (Addr host) =
  let req = N.defaultRequest { N.host = host
                             , N.secure = True
                             , N.path = fromString "/versions" }
  in N.withResponse req mgr $ \response -> _ response
