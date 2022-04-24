{-# LANGUAGE LambdaCase #-}
module Snap.Server.Utils.Shim where

import qualified Data.ByteString.Char8 as S
import Data.String (fromString)
import Snap.Core


-- | This functoin does the same as 'ipHeaderFilter', except that it also
-- allows `:` and `a-fA-F` in IPs. This is to support IPv6 addresses.
ipHeaderFilterSupportingIPv6 :: MonadSnap m => m ()
ipHeaderFilterSupportingIPv6 = do
  (getHeader (fromString "x-forwarded-for") <$> getRequest) >>= \case
    Nothing -> return ()
    Just str -> do
      modifyRequest $ \rq -> rq
        { rqClientAddr = S.takeWhile (`elem` ".:0123456789abcdefABCDEF")
                         . S.dropWhile (`elem` " \t\r\n")
                         $ str }
