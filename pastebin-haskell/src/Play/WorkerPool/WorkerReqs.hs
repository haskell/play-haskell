{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-| Intended to be imported as "Worker". -}
module Play.WorkerPool.WorkerReqs (
  Addr(..),
  getVersions,
  runJob,
) where

import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import Data.String (fromString)
import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Types.Method as N
import System.IO (hPutStrLn, stderr)

import PlayHaskellTypes
import PlayHaskellTypes.Sign (PublicKey, SecretKey)
import qualified PlayHaskellTypes.Sign as Sign


data Addr = Addr ByteString PublicKey
  deriving (Show, Eq, Ord)

getVersions :: N.Manager -> Addr -> IO (Maybe [Version])
getVersions mgr addr =
  fmap (\(VersionsResponse vs) -> vs) <$>
    sendMessageWithoutReq mgr addr (fromString "/versions")

runJob :: SecretKey -> N.Manager -> Addr -> RunRequest -> IO (Maybe RunResponse)
runJob skey mgr addr req =
  sendMessageWithReq skey mgr addr (fromString "POST") (fromString "/job") req

data Dict c a where
  Dict :: c a => Dict c a

sendMessageWithoutReq :: (J.FromJSON res, SigningBytes res)
                      => N.Manager -> Addr -> ByteString -> IO (Maybe res)
sendMessageWithoutReq mgr addr path =
  sendMessage' mgr addr (fromString "GET") path Nothing

sendMessageWithReq :: (J.ToJSON req, SigningBytes req, J.FromJSON res, SigningBytes res)
                   => SecretKey -> N.Manager -> Addr -> N.Method -> ByteString -> req -> IO (Maybe res)
sendMessageWithReq skey mgr addr method path req =
  sendMessage' mgr addr method path (Just (skey, req, Dict, Dict))

-- Given:
-- - A TLS manager
-- - The worker address to send to
-- - The HTTP method for the request
-- - The request path
-- - If a request body is to be sent: the secret key of the server, the request
--   body, and type class instances for the request body
-- Returns the response if successful.
sendMessage' :: forall req res.
                (J.FromJSON res, SigningBytes res)
             => N.Manager -> Addr -> N.Method -> ByteString
             -> Maybe (SecretKey, req, Dict J.ToJSON req, Dict SigningBytes req)
             -> IO (Maybe res)
sendMessage' mgr addr@(Addr host pkey) method path mpair =
  let reqbody = case mpair of
        Nothing -> mempty
        Just (skey, reqdata, Dict, Dict) -> J.encode $
          Message { sesmsgSignature = Sign.sign skey (signingBytes reqdata)
                  , sesmsgPublicKey = pkey
                  , sesmsgContent = reqdata }
      nreq = N.defaultRequest { N.host = host
                              , N.secure = True
                              , N.method = method
                              , N.path = path
                              , N.requestBody = N.RequestBodyLBS reqbody }
  in N.withResponse nreq mgr $ \response -> do
       body <- N.brReadSome (N.responseBody response) responseSizeLimitBytes
       case J.decode' body of
         Just (Message signature pkey' content)
           | pkey == pkey'
           , Sign.verify pkey (signingBytes content) signature
           -> return (Just content)
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
