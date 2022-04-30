module Snap.Server.Utils.Hex where

import qualified Data.ByteString.Short as BSS
import Data.Char (ord, chr)
import Data.Word (Word8)


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
