{-# LANGUAGE TemplateHaskell #-}
module Embed where

import Data.ByteString (ByteString)
import Data.FileEmbed


indexHTML :: ByteString
indexHTML = $(embedFile "index.html")

responseHTML :: ByteString
responseHTML = $(embedFile "response.html")

readHTML :: ByteString
readHTML = $(embedFile "read.html")
