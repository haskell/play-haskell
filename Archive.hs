module Archive (createArchive) where

import qualified Codec.Archive as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (chr, isAlphaNum, ord)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (POSIXTime)
import System.Posix.Types (CMode(..))
import Foreign.C.Types (CTime(..))


createArchive :: ByteString -> Maybe POSIXTime -> [(Maybe ByteString, ByteString)] -> Lazy.ByteString
createArchive key mtime files =
    let dirprefix = Char8.unpack key ++ "/"
        names = chooseNames (length dirprefix)
                            [(fromMaybe (Char8.pack "file.hs") name, content)
                            | (name, content) <- files]
        entries = [fileEntry (dirprefix ++ Char8.unpack name) mtime content
                  | (name, content) <- names]
    in GZip.compress (Tar.entriesToBSL entries)

fileEntry :: FilePath -> Maybe POSIXTime -> ByteString -> Tar.Entry FilePath ByteString
fileEntry fp modtime contents =
    Tar.Entry { Tar.filepath = fp
              , Tar.content = Tar.NormalFile contents
              , Tar.permissions = CMode 0o0644
              , Tar.ownership = Tar.Ownership Nothing Nothing 0 0
              , Tar.time = fmap (\t -> (CTime (floor t), 0)) modtime }

chooseNames :: Int -> [(ByteString, a)] -> [(ByteString, a)]
chooseNames reservedPrefixLen =
    concatMap numberGroup
    . groupBy ((==) `on` fst)
    . sortBy (comparing fst)
    . map (first (BS.take maxNameLen . filterName))
  where
    numberGroup :: [(ByteString, a)] -> [(ByteString, a)]
    numberGroup [file] = [file]
    numberGroup files =
        zipWith (\(file, tag) suf -> (addSuffix file suf, tag))
                files
                [Char8.pack ('-' : show i) | i <- [1::Int ..]]

    addSuffix :: ByteString -> ByteString -> ByteString
    addSuffix name suffix = case BS.elemIndexEnd (fromIntegral (ord '.')) name of
        Just idx -> let (prefix, extension) = BS.splitAt idx name
                    in BS.concat [BS.take (maxLen - BS.length suffix - BS.length extension) prefix
                                 ,suffix
                                 ,extension]
        Nothing -> BS.concat [BS.take (maxLen - BS.length suffix) name
                             ,suffix]
      where
        maxLen = maxNameLen - reservedPrefixLen

    filterName :: ByteString -> ByteString
    filterName name =
        let name' = BS.filter (\c -> 32 <= c && c < 127 &&
                                        (let ch = chr (fromIntegral c)
                                         in isAlphaNum ch || ch `elem` "-_."))
                              name
        in if BS.null name' then Char8.pack "file.hs" else name'

-- This is a limitation of the POSIX USTAR format as supported by tar-bytestring.
maxNameLen :: Int
maxNameLen = 100
