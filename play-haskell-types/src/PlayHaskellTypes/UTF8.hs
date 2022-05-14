module PlayHaskellTypes.UTF8 (
  JSONUTF8LBS(..),
) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding.Error as TEE


newtype JSONUTF8LBS = JSONUTF8LBS { getJSONUTF8LBS :: Lazy.ByteString }
  deriving (Show)

instance J.FromJSON JSONUTF8LBS where
  parseJSON (J.String txt) = return $ JSONUTF8LBS (Lazy.fromStrict (TE.encodeUtf8 txt))

  parseJSON val = J.prependFailure "parsing Lazy ByteString failed, " (J.typeMismatch "String" val)

instance J.ToJSON JSONUTF8LBS where
  toJSON (JSONUTF8LBS lbs) = J.String (TL.toStrict (TLE.decodeUtf8With TEE.lenientDecode lbs))

  toEncoding (JSONUTF8LBS lbs) = JE.lazyText (TLE.decodeUtf8With TEE.lenientDecode lbs)
