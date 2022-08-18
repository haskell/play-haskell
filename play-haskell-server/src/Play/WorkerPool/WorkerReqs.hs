{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Intended to be imported as "Worker". -}
module Play.WorkerPool.WorkerReqs (
  Addr(..),
  getHealth,
  runJob,
) where

import Control.Exception (handle)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Char (ord)
import Data.String (fromString)
import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Types.Status as N
import qualified Network.HTTP.Types.Method as N
import Text.Read (readMaybe)
import System.IO (hPutStrLn, stderr)

import PlayHaskellTypes
import PlayHaskellTypes.Sign (PublicKey, SecretKey)
import qualified PlayHaskellTypes.Sign as Sign
import Snap.Server.Utils.ExitEarly


data Addr = Addr ByteString PublicKey
  deriving (Show, Eq, Ord)

getHealth :: N.Manager -> Addr -> IO (Maybe HealthResponse)
getHealth mgr addr =
  sendMessageWithoutReq mgr addr (fromString "/health")

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
sendMessage' mgr addr@(Addr _ pkey) method path mpair =
  let (secure, host, port) = splitAddrHost addr
      reqbody = case mpair of
        Nothing -> mempty
        Just (skey, reqdata, Dict, Dict) -> J.encode $
          Message { sesmsgSignature = Sign.sign skey (signingBytes reqdata)
                  , sesmsgPublicKey = Sign.publicKey skey
                  , sesmsgContent = reqdata }
      nreq = N.defaultRequest { N.host = host
                              , N.port = port
                              , N.secure = secure
                              , N.method = method
                              , N.path = path
                              , N.requestBody = N.RequestBodyLBS reqbody }
  in handle (\(e :: N.HttpException) -> print e >> return Nothing) $
     N.withResponse nreq mgr $ \response -> execExitEarlyT $ do
       when (N.statusCode (N.responseStatus response) /= 200) $
         exitEarly Nothing

       body <- lift $ N.brReadSome (N.responseBody response) responseSizeLimitBytes
       case J.decode' body of
         Just (Message signature pkey' content)
           | pkey == pkey'
           , Sign.verify pkey (signingBytes content) signature
           -> return (Just content)
           | otherwise
           -> do if pkey == pkey'
                   then liftIO $ hPutStrLn stderr $ "Invalid signature from " ++ show addr
                   else liftIO $ hPutStrLn stderr $ "Unexpected pkey from " ++ show addr ++ ": " ++ show pkey'
                 return Nothing  -- invalid signature or unexpected public key
         Nothing -> do
           liftIO $ hPutStrLn stderr $ "Failed to decode JSON body from worker"
           return Nothing
  where
    splitAddrHost :: Addr -> (Bool, ByteString, Int)
    splitAddrHost (Addr address _) =
      let (secure, rest)
            | Just s <- BS.stripPrefix (Char8.pack "http://") address = (False, s)
            | otherwise = (True, address)
          colon = toEnum (ord ':')
      in if | toEnum (ord ':') `BS.elem` rest
            , Just port <- readMaybe (Char8.unpack (BS.takeWhileEnd (/= colon) rest)) ->
                (secure, BS.init (BS.dropWhileEnd (/= colon) rest), port)
            | otherwise -> (secure, rest, if secure then 443 else 80)

-- The max output size in the worker is 100_000 bytes, so allow twice that
-- (stdout + stderr) plus some overhead.
responseSizeLimitBytes :: Int
responseSizeLimitBytes = 220_000
