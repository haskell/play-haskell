{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
module PlayHaskellTypes.Statistics.Stats (
  -- * Statistics collector
  Stats,
  statsSingleton,

  -- ** Getting results
  statsCount,
  statsMean,
  statsMean',
  statsStddev,
  statsMin,
  statsMax,

  -- ** Pretty-printing stats
  prettyStats,

  -- * Histogram collector
  Histogram,
  histEmpty,
  histAdd,
  histFreeze,
  histPretty,
) where

import qualified Data.IntMap.Strict as IMap
import Data.IntMap.Strict (IntMap)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid
import Data.Semigroup
import GHC.Generics (Generic, Generically(..))
import Numeric

import PlayHaskellTypes.Statistics.Types


data Stats a = Stats
  { statsSum :: !(Sum a)
  , statsSumSq :: !(Sum a)
  , statsCount' :: !(Sum Int)
  , statsMin' :: !(Maybe' (Min a))
  , statsMax' :: !(Maybe' (Max a)) }
  deriving (Generic)
  deriving (Semigroup, Monoid) via Generically (Stats a)

statsCount :: Stats a -> Int
statsCount = getSum . statsCount'

statsSingleton :: forall a. (Num a, Ord a) => a -> Stats a
statsSingleton x = Stats
  { statsSum = Sum x
  , statsSumSq = Sum (x * x)
  , statsCount' = 1
  , statsMin' = Just' (Min x)
  , statsMax' = Just' (Max x) }

statsMean' :: Fractional b => (a -> b) -> Stats a -> b
statsMean' f s = f (getSum (statsSum s)) / fromIntegral (statsCount s)

statsMean :: Fractional a => Stats a -> a
statsMean = statsMean' id

-- SampleVar(X)
--   = Var(X) * BesselCorrection(n)
--   = E[(X - E[X])^2] * (n / (n - 1))
--   = (E[X^2] - E[X]^2) * (n / (n - 1))
--   = ((sum x_i^2)/n - (sum x_i / n)^2) * (n / (n - 1))
--   = (sum x_i^2)/(n-1) - (sum x_i)^2 / (n(n-1))
-- SampleStdDev(X) = sqrt(SampleVar(X))
statsStddev :: (Ord a, Floating a) => Stats a -> a
statsStddev s =
  let sumx = getSum (statsSum s)
      sumsq = getSum (statsSumSq s)
      n = statsCount s
  in sqrt $ max 0 $ sumsq / fromIntegral (n - 1) - sumx ^ (2::Int) / fromIntegral (n * (n - 1))

-- | Returns 'Nothing' if dataset is empty.
statsMin :: Stats a -> Maybe a
statsMin s = case statsMin' s of
               Nothing' -> Nothing
               Just' (Min x) -> Just x

-- | Returns 'Nothing' if dataset is empty.
statsMax :: Stats a -> Maybe a
statsMax s = case statsMax' s of
               Nothing' -> Nothing
               Just' (Max x) -> Just x

prettyStats :: forall a. (Ord a, RealFloat a) => Stats a -> String
prettyStats s
  | statsCount s == 0 = "<no data>"
  | otherwise =
      showAt3 (statsMean s) ++ " ± " ++ showAt3 (statsStddev s)
      ++ " in [" ++ showAt3 (fromJust (statsMin s)) ++ ", " ++ showAt3 (fromJust (statsMax s)) ++ "]"


data Histogram a =
  Histogram { hLo :: a
            , hHi :: a
            , hNbins :: Int
            , hBelowStats :: Stats a
            , hHistogram :: IntMap Int
            , hAboveStats :: Stats a
            }

histEmpty :: (Num a, Ord a) => a -> a -> Int -> Histogram a
histEmpty lo hi nbins = Histogram
  { hLo = lo
  , hHi = hi
  , hNbins = nbins
  , hBelowStats = mempty
  , hHistogram = mempty
  , hAboveStats = mempty }

histAdd :: RealFrac a => a -> Histogram a -> Histogram a
histAdd value h
  | value < hLo h = h { hBelowStats = statsSingleton value <> hBelowStats h }
  | value > hHi h = h { hAboveStats = statsSingleton value <> hAboveStats h }
  | otherwise =
      let idx = min (hNbins h) . max 0 $  -- it's already in range, so any out-of-range values here are float inaccuracies
                  floor ((value - hLo h) / (hHi h - hLo h) * fromIntegral (hNbins h))
          hist' = IMap.insertWith (+) idx 1 (hHistogram h)
      in h { hHistogram = hist' }

histFreeze :: Histogram a -> (a, a, Stats a, [Int], Stats a)
histFreeze h =
  (hLo h
  ,hHi h
  ,hBelowStats h
  ,[fromMaybe 0 (IMap.lookup i (hHistogram h)) | i <- [0 .. hNbins h - 1]]
  ,hAboveStats h)

histPretty :: RealFloat a => Histogram a -> String
histPretty h =
  let (lo, hi, bstat, hist, astat) = histFreeze h
      most = if null hist then 0 else maximum hist
      -- The zero value is not a space so that a bar graph still makes some
      -- sense in a non-monospace setting.
      -- The 9 is to ensure 1/8th of the range actually goes to the largest bar.
      char n = "▏▁▂▃▄▅▆▇█" !! (min 8 (n * 9 `div` most))
  in showAt3 lo
     ++ (if statsCount bstat > 0 then "(" ++ prettyStats bstat ++ ")" else "")
     ++ "[" ++ map char hist ++ "]" 
     ++ (if statsCount astat > 0 then "(" ++ prettyStats astat ++ ")" else "")
     ++ showAt3 hi


showAt3 :: RealFloat a => a -> String
showAt3 f = showGFloat (Just 3) f ""
