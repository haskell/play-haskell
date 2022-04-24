{-# LANGUAGE NumericUnderscores #-}
{-| A key that refreshes once in a while.

Create a new self-refreshing key using 'makeRefreshingChallenge';
'servingChallenge' then returns the current text to be given to clients.
'checkChallenge' returns whether the given text is equal to either the current
challenge or the previous one.
-}
module Snap.Server.Utils.Challenge (
  ChallengeKey,
  makeRefreshingChallenge,
  servingChallenge,
  checkChallenge,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Char
import Control.Monad (forever)
import qualified Data.ByteString.Short as BSS
import qualified Data.Text as T
import Data.Time
import Data.Word (Word8)
import System.Random


-- (previous, current)
newtype ChallengeKey = ChallengeKey (TVar (BSS.ShortByteString, BSS.ShortByteString))

-- | The previous challenge key is considered correct by 'checkChallenge'.
makeRefreshingChallenge :: DiffTime -> IO ChallengeKey
makeRefreshingChallenge refreshInterval = do
  initchs <- (,) <$> generateChallenge <*> generateChallenge
  var <- newTVarIO initchs
  let key = ChallengeKey var
  _ <- forkIO $ forever $ do
    threadDelay (fromIntegral $ diffTimeToPicoseconds refreshInterval `div` 1000_000)
    ch <- generateChallenge
    atomically $ modifyTVar' var (\(_, old) -> (old, ch))
  return key

servingChallenge :: ChallengeKey -> IO T.Text
servingChallenge (ChallengeKey var) = bss2text . snd <$> readTVarIO var

checkChallenge :: ChallengeKey -> T.Text -> IO Bool
checkChallenge (ChallengeKey var) test = do
  let test' = text2bss test
  (prev, cur) <- readTVarIO var
  return (test' == prev || test' == cur)

generateChallenge :: IO BSS.ShortByteString
generateChallenge = do
  gen <- newStdGen
  let n :: Integer
      (n, _) = uniformR (0, radix ^ numchars - 1) gen
      digits :: [Int]
      digits = take numchars
               . map (fromIntegral . (`mod` radix))
               . iterate (`div` radix)
               $ n
  return (BSS.pack (map char digits))
  where
    radix = 2 * 26 :: Integer
    numchars = 12 :: Int
    char :: Int -> Word8
    char i | i < 26 = fromIntegral (ord 'a' + i)
           | otherwise = fromIntegral (ord 'A' + i - 26)

bss2text :: BSS.ShortByteString -> T.Text
bss2text = T.pack . map (chr . fromIntegral) . BSS.unpack

text2bss :: T.Text -> BSS.ShortByteString
text2bss = BSS.pack . map (fromIntegral . ord) . T.unpack
