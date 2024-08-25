{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
module Statistics.Stats (
  Stats,
  oneStat,
  statsCount,
  statsMean,
  statsStddev,
  statsMin,
  statsMax,
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Semigroup


data SBool b where
  STrue :: SBool True
  SFalse :: SBool False

class KnownBool b where knownBool :: SBool b
instance KnownBool True where knownBool = STrue
instance KnownBool False where knownBool = SFalse

type family If a b c where
  If True b c = b
  If False b c = c

mkIf :: forall b c a. SBool a -> b -> c -> If a b c
mkIf STrue x _ = x
mkIf SFalse _ y = y


-- | Strict 'Maybe'.
data Maybe' a = Nothing' | Just' !a
  deriving (Show)

instance Semigroup a => Semigroup (Maybe' a) where
  Nothing' <> x = x
  x <> Nothing' = x
  Just' x <> Just' y = Just' (x <> y)

instance Semigroup a => Monoid (Maybe' a) where
  mempty = Nothing'

-- | Strict pair.
data Pair' a b = Pair' !a !b
  deriving (Show)


-- An online histogram collector. Bins are equally spaced intervals in [min, max).
data RunningHist a
  = RunningHist !Int                   -- ^ target number of bins
                !Int                   -- ^ actual number of bins
                !(Maybe' (Pair' a a))  -- ^ (min, max)
                !(Map Int Int)         -- ^ the bins

rhistInit :: Int -> RunningHist a
rhistInit n = RunningHist n 0 Nothing' mempty

rhistAdd :: forall a. (RealFrac a, Ord a) => a -> RunningHist a -> RunningHist a
rhistAdd !x (RunningHist targetn _ Nothing' _) =
  RunningHist targetn 1 (Just' (Pair' x x)) (Map.singleton 0 1)
rhistAdd x (RunningHist targetn actualn bds@(Just' (Pair' mi ma)) mp)
  | mi <= x, x < ma =
      let idx | ma <= mi = 0
              | otherwise = min (actualn - 1)  -- just to be sure...
                                (floor ((x - mi) * fromIntegral actualn / (ma - mi)))
      in RunningHist targetn actualn bds
                     (Map.insertWith (+) idx 1 mp)
  | otherwise =
      let currange = ma - mi
          newrange = max ma x - min mi x
      in -- If the new range is so much larger that everything fits in one
         -- bin (with target number of bins), just do that.
         -- Do not compute with binwidth here, it may be infinite or ridiculously large!
         if currange <= 0 || newrange / currange >= fromIntegral targetn
           then -- put all existing data in one bin
             if x < mi
               then RunningHist targetn targetn (Just' (Pair' x ma))
                                (Map.fromList [(0, 1), (targetn - 1, sum (Map.elems mp))])
               else -- Make x the _start_ of the last bin, because the histogram's range is [min, max)
                    let newma = x + (x - mi) / fromIntegral (targetn - 1)
                    in RunningHist targetn targetn (Just' (Pair' mi newma))
                                   (Map.fromList [(0, sum (Map.elems mp)), (targetn - 1, 1)])


           -- If the new range would yield at least (roughly) twice the target
           -- number of bins, redistribute.
           -- Because the indices are not too large (<= targetn^2), so we can
           -- sensibly compute with the bin width here.
           else
             let binwid = (ma - mi) / fromIntegral actualn
                 nbefore = ceiling ((mi - min mi x) / binwid)
                 mi' = mi - fromIntegral nbefore * binwid
                 nafter | x < ma = 0
                        | otherwise = floor ((max ma x - ma) / binwid + 1)
                 ma' = ma + fromIntegral nafter * binwid
                 newnbins = nbefore + actualn + nafter
                 reduction = newnbins `quot` targetn
                 idx = floor ((x - mi') / (binwid / fromIntegral reduction))
             in RunningHist targetn
                            newnbins
                            (Just' (Pair' mi' ma'))
                            (Map.insertWith (+) idx 1
                              (Map.mapKeysWith (+) (\i -> (i + nbefore) `quot` reduction) mp))


data StatOptions = StatOptions Bool Bool Bool
type family StatOpMean   op where StatOpMean   ('StatOptions b _ _) = b
type family StatOpStddev op where StatOpStddev ('StatOptions _ b _) = b
type family StatOpHist   op where StatOpHist   ('StatOptions _ _ b) = b

class (KnownBool (StatOpMean op), KnownBool (StatOpStddev op), KnownBool (StatOpHist op)) => KnownStatOptions op
instance (KnownBool m, KnownBool s, KnownBool h) => KnownStatOptions ('StatOptions m s h)

data Stats (op :: StatOptions) a = Stats
  { statsSum :: !(If (StatOpMean op) (Sum a) ())
  , statsSumSq :: !(If (StatOpStddev op) (Sum a) ())
  , statsCount :: !(Sum Int)
  , statsMin :: !(Maybe' (Min a))
  , statsMax :: !(Maybe' (Max a)) }

instance (Num a, Ord a, KnownStatOptions op) => Semigroup (Stats op a) where
  Stats sx sx2 n mi ma <> Stats sx' sx2' n' mi' ma' =
    Stats (sappendIf @(Sum a) @() (knownBool @(StatOpMean op)) sx sx')
          (sappendIf @(Sum a) @() (knownBool @(StatOpStddev op)) sx2 sx2')
          (n <> n') (mi <> mi') (ma <> ma')
    where
      -- This type is ambiguous; use type applications.
      sappendIf :: forall b c a'. (Semigroup b, Semigroup c) => SBool a' -> If a' b c -> If a' b c -> If a' b c
      sappendIf STrue x y = x <> y
      sappendIf SFalse x y = x <> y

instance (Num a, Ord a, KnownStatOptions op) => Monoid (Stats op a) where
  mempty = Stats (mkIf @(Sum a) (knownBool @(StatOpMean op)) (Sum 0) ())
                 (mkIf @(Sum a) (knownBool @(StatOpStddev op)) (Sum 0) ())
                 0 Nothing' Nothing'

oneStat :: forall a op. (Num a, KnownStatOptions op) => a -> Stats op a
oneStat x = Stats
  { statsSum = mkIf (knownBool @(StatOpMean op)) (Sum x) ()
  , statsSumSq = mkIf (knownBool @(StatOpStddev op)) (Sum (x * x)) ()
  , statsCount = 1
  , statsMin = Just' (Min x)
  , statsMax = Just' (Max x) }

statsMean :: (Fractional a, StatOpMean op ~ True) => Stats op a -> a
statsMean s = getSum (statsSum s) / fromIntegral (getSum (statsCount s))

-- SampleVar(X)
--   = Var(X) * BesselCorrection(n)
--   = E[(X - E[X])^2] * (n / (n - 1))
--   = (E[X^2] - E[X]^2) * (n / (n - 1))
--   = ((sum x_i^2)/n - (sum x_i / n)^2) * (n / (n - 1))
--   = (sum x_i^2)/(n-1) - (sum x_i)^2 / (n(n-1))
-- SampleStdDev(X) = sqrt(SampleVar(X))
statsStddev :: (Floating a, StatOpMean op ~ True, StatOpStddev op ~ True) => Stats op a -> a
statsStddev s =
  let sumx = getSum (statsSum s)
      sumsq = getSum (statsSumSq s)
      n = getSum (statsCount s)
  in sqrt $ sumsq / fromIntegral (n - 1) - sumx ^ (2::Int) / fromIntegral (n * (n - 1))
