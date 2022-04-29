module PlayHaskellTypes.Sign (
  -- * Data types
  PublicKey(..),
  SecretKey,
  Signature(..),
  readSecretKey,
  revealSecretKey,
  publicKey,

  -- * Signing
  sign,
  verify,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteArray as Mem
import qualified Crypto.Error as Cr
import qualified Crypto.PubKey.Ed25519 as Cr


-- | 32 bytes.
newtype PublicKey = PublicKey BSS.ShortByteString
  deriving (Show)

-- | Generate a secret key by applying 'readSecretKey' to 32 random bytes.
--
-- Note: The 'Show' instance doesn't actually show the secret key itself.
newtype SecretKey = SecretKey Cr.SecretKey
  deriving (Show)

-- | 64 bytes.
newtype Signature = Signature BSS.ShortByteString
  deriving (Show)

-- | Returns 'Nothing' if the input is not 32 bytes long.
readSecretKey :: ByteString -> Maybe SecretKey
readSecretKey bs = case Cr.secretKey bs of
                     Cr.CryptoPassed key -> Just (SecretKey key)
                     Cr.CryptoFailed{} -> Nothing

-- | Use this function with care; do this only in a short-lived process that
-- puts the revealed key in a good place. The output is 32 bytes.
revealSecretKey :: SecretKey -> ByteString
revealSecretKey (SecretKey key) = BS.pack (Mem.unpack key)

-- | Return the public key corresponding to a secret key.
publicKey :: SecretKey -> PublicKey
publicKey (SecretKey key) = PublicKey (BSS.pack (Mem.unpack (Cr.toPublic key)))

-- | The 'ByteString' is the message to sign.
sign :: SecretKey -> ByteString -> Signature
sign (SecretKey skey) msg =
  Signature . BSS.pack . Mem.unpack $ Cr.sign skey (Cr.toPublic skey) msg

-- | The 'ByteString' is the message to sign.
verify :: PublicKey -> ByteString -> Signature -> Bool
verify (PublicKey pkey) msg (Signature sgn) =
  case (,) <$> Cr.publicKey (BS.pack (BSS.unpack pkey))
           <*> Cr.signature (BS.pack (BSS.unpack sgn)) of
    Cr.CryptoPassed (pkey', sgn') -> Cr.verify pkey' msg sgn'
    Cr.CryptoFailed{} -> error "Invalid public key or signature given to 'verify'"
