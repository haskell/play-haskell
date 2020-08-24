module Persist (
    Persistable(..)
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Short as Short


class Persistable a where
    persist :: a -> Builder.Builder
    parse :: ByteString -> (a, ByteString)

instance Persistable ByteString where
    persist bs = writeSize (BS.length bs) <> Builder.byteString bs
    parse bs = uncurry BS.splitAt (parseSize bs)

instance Persistable Short.ShortByteString where
    persist bs = writeSize (Short.length bs) <> Builder.shortByteString bs
    parse bs =
        let (content, res) = uncurry BS.splitAt (parseSize bs)
        in (Short.toShort content, res)

instance (Persistable a, Persistable b) => Persistable (a, b) where
    persist (x, y) = persist x <> persist y
    parse bs = let (x, (y, rest)) = parse <$> parse bs
               in ((x, y), rest)

instance Persistable a => Persistable [a] where
    persist l = writeSize (length l) <> foldMap persist l
    parse bs =
        let parseN 0 bs' = ([], bs')
            parseN m bs' = let (x, bs'') = parse bs'
                               (xs, rest) = parseN (m - 1) bs''
                           in (x : xs, rest)
        in uncurry parseN (parseSize bs)

writeSize :: Int -> Builder.Builder
writeSize = Builder.int64LE . fromIntegral

parseSize :: ByteString -> (Int, ByteString)
parseSize bs =
    let (prefix, rest) = BS.splitAt 8 bs
        n = sum [2 ^ (8 * i) * fromIntegral (prefix `BS.index` i) | i <- [0..7]]
    in (n, rest)
