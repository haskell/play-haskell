{-# LANGUAGE BangPatterns #-}
module PlayHaskellTypes.Statistics.RunningHist where

import qualified Data.IntMap.Strict as IMap
import Data.IntMap.Strict (IntMap)
import Data.Maybe (fromMaybe)

import PlayHaskellTypes.Statistics.Types


-- An online histogram collector. Bins are equally spaced intervals in [min, max).
-- Invariant: if min == max, then all data points are equal and actualn == 1.
data RunningHist a
  = RunningHist !Int                   -- ^ target number of bins
                !Int                   -- ^ actual number of bins
                !(Maybe' (Pair' a a))  -- ^ (min, max)
                !(IntMap Int)          -- ^ the bins
  deriving (Show)

rhistInit :: Int -> RunningHist a
rhistInit n = RunningHist n 0 Nothing' mempty

rhistAdd :: RealFrac a => a -> RunningHist a -> RunningHist a
rhistAdd !x (RunningHist targetn _ Nothing' _) =
  RunningHist targetn 1 (Just' (Pair' x x)) (IMap.singleton 0 1)
rhistAdd x (RunningHist targetn actualn bds@(Just' (Pair' mi ma)) mp)
  | (mi <= x && x < ma) || (mi == x && x == ma) =
      let idx | ma <= mi = 0
              | otherwise = min (actualn - 1)  -- just to be sure...
                                (floor ((x - mi) * fromIntegral actualn / (ma - mi)))
      in RunningHist targetn actualn bds
                     (IMap.insertWith (+) idx 1 mp)
  | otherwise =
      let currange = ma - mi
          newrange = max ma x - min mi x
      in -- If the new range is so much larger that everything fits in one
         -- bin (with target number of bins), just do that.
         -- Do not compute with binwidth here, it may be infinite or ridiculously large!
         if currange <= 0 || newrange / currange >= fromIntegral targetn
           then -- put all existing data in one bin
             if x < mi
               then -- If mi==ma, make mi the midpoint of the last bin, because the histogram's range is [min, max)
                    let newma | currange <= 0 = mi + (mi - x) / fromIntegral (2 * targetn - 1)
                              | otherwise = ma
                    in RunningHist targetn targetn (Just' (Pair' x newma))
                                   (IMap.fromList [(0, 1), (targetn - 1, sum (IMap.elems mp))])
               else -- Make x the midpoint of the last bin, because the histogram's range is [min, max)
                    let newma = x + (x - mi) / fromIntegral (2 * targetn - 1)
                    in RunningHist targetn targetn (Just' (Pair' mi newma))
                                   (IMap.fromList [(0, sum (IMap.elems mp)), (targetn - 1, 1)])

           -- Redistribute. Ensure that the number of bins stays below
           -- 2*targetn.
           -- Because the indices are not too large (<= targetn^2), we can
           -- sensibly compute with the bin width here.
           else
             let binwid = (ma - mi) / fromIntegral actualn
                 nbefore = ceiling ((mi - min mi x) / binwid)
                 mi' = mi - fromIntegral nbefore * binwid
                 nafter | x < ma = 0
                        | otherwise =
                            let candidate = floor ((max ma x - ma) / binwid + 1)
                            in if ma + fromIntegral candidate * binwid <= x
                                 then candidate + 1 else candidate  -- catch rounding errors
                 ma' = ma + fromIntegral nafter * binwid
                 newnbinsPrecise = nbefore + actualn + nafter
                 reduction = newnbinsPrecise `quot` targetn
                 newnbins = (newnbinsPrecise + reduction - 1) `quot` reduction
                 idx = floor ((x - mi') / binwid) `quot` reduction
             in RunningHist targetn
                            newnbins
                            (Just' (Pair' mi' ma'))
                            (IMap.insertWith (+) idx 1
                              (IMap.mapKeysWith (+) (\i -> (i + nbefore) `quot` reduction) mp))

rhistFinish :: RunningHist a -> Maybe (a, a, [Int])
rhistFinish (RunningHist _ _ Nothing' _) = Nothing
rhistFinish (RunningHist _ actualn (Just' (Pair' mi ma)) mp) =
  Just (mi, ma, [fromMaybe 0 (IMap.lookup i mp) | i <- [0 .. actualn-1]])
