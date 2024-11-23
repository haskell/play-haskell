{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Play.Examples (exampleSnippets, randomExampleSnippet) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Foldable (toList)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector as V
import Data.Vector (Vector)
import System.Random.Stateful (randomRM, globalStdGen)


randomExampleSnippet :: IO ByteString
randomExampleSnippet = do
  index <- randomRM (0, V.length exampleSnippets - 1) globalStdGen
  return (exampleSnippets V.! index)

exampleSnippets :: Vector ByteString
exampleSnippets = parseSnippetsFile $(embedFileRelative "example-snippets.txt")

parseSnippetsFile :: ByteString -> Vector ByteString
parseSnippetsFile file =
  let lns = -- filter out comments
            filter (\line -> not (line `startsWith` BS8.pack "-- ~~~~ # ")) $
              BS.split 10 file
  in V.fromList $ map trimFile $ toList $ splitOn (BS8.pack "-- ~~~~ CUT") lns
  where
    a `startsWith` b = BS.take (BS.length b) a == b

    splitOn _ [] = [] :| []
    splitOn spl (x:xs) = let chunk :| chunks = splitOn spl xs
                         in if x == spl then [] :| (chunk : chunks)
                                        else (x : chunk) :| chunks

    trimFile = BS8.intercalate (BS.singleton 10) . dropWhileEnd BS.null . dropWhile BS.null
