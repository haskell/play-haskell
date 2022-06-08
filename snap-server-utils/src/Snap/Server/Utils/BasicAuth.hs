module Snap.Server.Utils.BasicAuth where

import Control.Monad ((>=>), guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char (ord)
import Data.String (fromString)
import Snap.Core hiding (pass)


-- | Set the response code to 401 and add a @WWW-Authenticate@ header to a
-- request with the given realm string (description of the authentication
-- domain). Use as @'modifyResponse' (requireBasicAuth "admin area")@.
-- Throws an error if the realm string has disallowed characters.
requireBasicAuth :: String -> Response -> Response
requireBasicAuth realm
  -- This format is more strict than it needs to be: quotes and backslashes can
  -- be escaped, and horizontal tabs are allowed.
  -- https://httpwg.org/specs/rfc7230.html#field.components
  -- However, I don't trust browsers.
  | all (\c -> c `notElem` "\"\\\DEL" && c >= ' ' && c < '\DEL') realm =
      setResponseCode 401 .
      addHeader (fromString "WWW-Authenticate")
        (UTF8.fromString $ "realm=\"" ++ realm ++ "\", charset=\"UTF-8\"")
  | otherwise = error "requireBasicAuth: invalid characters in realm string"

-- | Get submitted username and password from a response, if given. Use as
-- @'getBasicAuthCredentials' <$> 'getRequest'@.
getBasicAuthCredentials :: Request -> Maybe (ByteString, ByteString)
getBasicAuthCredentials =
  getHeader (fromString "Authorization") >=> \text -> do
    [basic, b64] <- return (filter (not . BS.null) (BS.split 32 text))
    guard (basic == fromString "Basic")
    Right dec <- return (Base64.decode b64)
    let (user, passWithColon) = BS.break (== fromIntegral (ord ':')) dec
    Just (_, pass) <- return (BS.uncons passWithColon)
    return (user, pass)
