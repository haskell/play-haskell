module EmbedRuntime where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import System.IO.Unsafe


indexHTML :: ByteString
indexHTML = unsafePerformIO (Char8.pack <$> readFile "index.html")

responseHTML :: ByteString
responseHTML = unsafePerformIO (Char8.pack <$> readFile "response.html")

readHTML :: ByteString
readHTML = unsafePerformIO (Char8.pack <$> readFile "read.html")
