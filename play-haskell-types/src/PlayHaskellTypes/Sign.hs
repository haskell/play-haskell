{-# LANGUAGE ViewPatterns #-}
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
import Data.Char (ord, chr)
import Data.Word (Word8)
import qualified Crypto.Error as Cr
import qualified Crypto.PubKey.Ed25519 as Cr
import Text.JSON (JSON)
import qualified Text.JSON as JSON


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

-- | The 'ByteString' is the message that was signed.
verify :: PublicKey -> ByteString -> Signature -> Bool
verify (PublicKey pkey) msg (Signature sgn) =
  case (,) <$> Cr.publicKey (BS.pack (BSS.unpack pkey))
           <*> Cr.signature (BS.pack (BSS.unpack sgn)) of
    Cr.CryptoPassed (pkey', sgn') -> Cr.verify pkey' msg sgn'
    Cr.CryptoFailed{} -> error "Invalid public key or signature given to 'verify'"

instance JSON PublicKey where
  readJSON (JSON.JSString (JSON.fromJSString -> s)) =
    case hexDecode s of
      Just sbs | BSS.length sbs == Cr.publicKeySize -> return (PublicKey sbs)
      _ -> fail "Invalid hex string in PublicKey JSON"
  readJSON _ = fail "Unable to read PublicKey"

  showJSON (PublicKey sbs) = JSON.JSString (JSON.toJSString (hexEncode sbs))

instance JSON Signature where
  readJSON (JSON.JSString (JSON.fromJSString -> s)) =
    case hexDecode s of
      Just sbs | BSS.length sbs == Cr.signatureSize -> return (Signature sbs)
      _ -> fail "Invalid hex string in Signature JSON"
  readJSON _ = fail "Unable to read Signature"

  showJSON (Signature sbs) = JSON.JSString (JSON.toJSString (hexEncode sbs))

hexDecode :: String -> Maybe BSS.ShortByteString
hexDecode s = BSS.pack <$> go s
  where
    go "" = Just []
    go (c1:c2:cs) = (:) <$> ((+) <$> digit c1 <*> digit c2) <*> go cs
    go _ = Nothing

    digit :: Char -> Maybe Word8
    digit c | ord '0' <= ord c, ord c <= ord '9' = Just $ fromIntegral $ ord c - ord '0'
            | ord 'a' <= ord c, ord c <= ord 'f' = Just $ fromIntegral $ ord c - ord 'a' + 10
            | ord 'A' <= ord c, ord c <= ord 'F' = Just $ fromIntegral $ ord c - ord 'A' + 10
            | otherwise = Nothing

hexEncode :: BSS.ShortByteString -> String
hexEncode = concatMap go . BSS.unpack
  where go n = [digit (n `div` 16), digit (n `mod` 16)]
        digit n | n < 10 = chr (ord '0' + fromIntegral n)
                | otherwise = chr (ord 'a' + fromIntegral n - 10)
