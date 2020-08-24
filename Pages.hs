module Pages (
    Pages(..), pagesFromDisk
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8


data Pages = Pages { pIndex :: ByteString
                   , pPasteRead :: (ByteString, ByteString, ByteString) }

pagesFromDisk :: IO Pages
pagesFromDisk = Pages <$> readIndexHTML <*> readPasteReadHTML

readIndexHTML :: IO ByteString
readIndexHTML = Char8.pack <$> readFile "index.html"

readPasteReadHTML :: IO (ByteString, ByteString, ByteString)
readPasteReadHTML = do
    fileContents <- readFile "read.html"
    let marker1 = "[[[INSERT_KEY]]]"
        marker2 = "[[[INSERT_CONTENTS]]]"
        idx1 = findSubString fileContents marker1
        idx2 = idx1 + findSubString (drop idx1 fileContents) marker2
    return (Char8.pack (take idx1 fileContents)
           ,Char8.pack (drop (idx1 + length marker1) (take idx2 fileContents))
           ,Char8.pack (drop (idx2 + length marker2) fileContents))

findSubString :: String -> String -> Int
findSubString [] _ = error "findSubString: no such substring"
findSubString large small
  | take (length small) large == small = 0
  | otherwise = 1 + findSubString (tail large) small
