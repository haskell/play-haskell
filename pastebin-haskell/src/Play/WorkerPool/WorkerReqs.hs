{-# LANGUAGE NumericUnderscores #-}
{-| Intended to be imported as "Worker". -}
module Play.WorkerPool.WorkerReqs where

import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import Data.String (fromString)
import qualified Network.HTTP.Client as N
import System.IO (hPutStrLn, stderr)

import PlayHaskellTypes
import PlayHaskellTypes.Sign (PublicKey)
import qualified PlayHaskellTypes.Sign as Sign


data Addr = Addr ByteString PublicKey
  deriving (Show, Eq, Ord)

getVersions :: N.Manager -> Addr -> IO (Maybe [Version])
getVersions mgr addr@(Addr host pkey) =
  let req = N.defaultRequest { N.host = host
                             , N.secure = True
                             , N.path = fromString "/versions" }
  in N.withResponse req mgr $ \response -> do
       body <- N.brReadSome (N.responseBody response) responseSizeLimitBytes
       case J.decode' body of
         Just (Message signature pkey' content@(VersionsResponse versions))
           | pkey == pkey'
           , Sign.verify pkey (signingBytes content) signature
           -> return (Just versions)
           | otherwise
           -> do if pkey == pkey'
                   then hPutStrLn stderr $ "Invalid signature from " ++ show addr
                   else hPutStrLn stderr $ "Unexpected pkey from " ++ show addr ++ ": " ++ show pkey'
                 return Nothing  -- invalid signature or unexpected public key
         Nothing -> return Nothing

-- The max output size in the worker is 100_000 bytes, so allow twice that
-- (stdout + stderr) plus some overhead.
responseSizeLimitBytes :: Int
responseSizeLimitBytes = 220_000
