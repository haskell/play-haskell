{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Pages (
  Pages(..), pagesFromDisk
) where

import PlayHaskellTypes
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.Char (ord, chr)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Encoding.Error as Enc (lenientDecode)
import System.Exit (die)
import qualified Text.Mustache as Mustache
import Text.Mustache (toMustache)
import qualified Text.Mustache.Types as Mustache (Value)


data Pages = Pages { pPlay :: Maybe Paste -> ByteString }

pagesFromDisk :: IO Pages
pagesFromDisk = Pages <$> (renderPlayPage <$> loadTemplate "play.mustache")

loadTemplate :: FilePath -> IO Mustache.Template
loadTemplate fp = do
  res <- Mustache.localAutomaticCompile fp
  case res of
    Right templ -> return templ
    Left err -> die (show err)

renderPlayPage :: Mustache.Template -> Maybe Paste -> ByteString
renderPlayPage templ = \case
  Just paste -> Enc.encodeUtf8 $ Mustache.substituteValue templ $ pasteToMustacheObject paste
  Nothing -> Enc.encodeUtf8 $ Mustache.substituteValue templ $ Mustache.object [(Text.pack "preload", toMustache False)]

pasteToMustacheObject :: Paste -> Mustache.Value
pasteToMustacheObject (Paste (Version version) contents _ _) = Mustache.object l
  where
    l = [(Text.pack "preload", mixinMaybeNull (jsStringEncode . decodeUtf8) msource),
        (Text.pack "ghc_version", toMustache $ jsStringEncode (Text.pack version))]
    msource = case contents of
       ((_, source) : _) -> Just source
       _ -> Nothing



mixinMaybeNull :: Mustache.ToMustache b => (a -> b) -> Maybe a -> Mustache.Value
mixinMaybeNull _ Nothing = toMustache False
mixinMaybeNull f (Just x) = toMustache (f x)

decodeUtf8 :: ByteString -> Text
decodeUtf8 = Enc.decodeUtf8With Enc.lenientDecode

-- | Puts quotes around the text and escapes the inside, so that the result is
-- a valid JS string that evaluates to the argument.
jsStringEncode :: Text -> Text
jsStringEncode text =
  let inner = flip Text.concatMap text $ \case
                '\n' -> Text.pack "\\n"
                '\t' -> Text.pack "\\t"
                '\\' -> Text.pack "\\\\"
                '"' -> Text.pack "\\\""
                c | ord c < 32 -> Text.pack ("\\x" ++ toHexN 2 (ord c))
                  | ord c < 127 -> Text.singleton c
                  | ord c >= 127, ord c <= 0xffff -> Text.pack ("\\u" ++ toHexN 4 (ord c))
                  | otherwise -> Text.pack ("\\u{" ++ toHexN 6 (ord c) ++ "}")
  in Text.singleton '"' <> inner <> Text.singleton '"'
  where
    toHexN nibbles n = [toHex1 ((n `shiftR` (4 * i)) `mod` 16) | i <- [nibbles - 1, nibbles - 2 .. 0]]
    toHex1 n | n < 10 = chr (ord '0' + n)
             | n < 16 = chr (ord 'a' + n - 10)
             | otherwise = error "Invalid"
