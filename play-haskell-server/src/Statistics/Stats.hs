{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Statistics.Stats (
  -- * Statistics collector
  Stats,
  StatOptions(..),
  statsEmpty,
  statsAdd,

  -- * Getting results
  statsCount,
  statsMean,
  statsMean',
  statsStddev,
  statsMin,
  statsMax,
  statsHist,

  -- * Pretty-printing stats
  prettyStats,
) where

import Data.List (transpose)
import qualified Data.IntMap.Strict as IMap
import Data.IntMap.Strict (IntMap)
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)
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

-- | This type is ambiguous, use type applications.
mkIf :: forall b c a. SBool a -> (a ~ True => b) -> (a ~ False => c) -> If a b c
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
                !(IntMap Int)          -- ^ the bins
  deriving (Show)

rhistInit :: Int -> RunningHist a
rhistInit n = RunningHist n 0 Nothing' mempty

rhistAdd :: forall a. RealFrac a => a -> RunningHist a -> RunningHist a
rhistAdd !x (RunningHist targetn _ Nothing' _) =
  RunningHist targetn 1 (Just' (Pair' x x)) (IMap.singleton 0 1)
rhistAdd x (RunningHist targetn actualn bds@(Just' (Pair' mi ma)) mp)
  | mi <= x, x < ma =
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
               then RunningHist targetn targetn (Just' (Pair' x ma))
                                (IMap.fromList [(0, 1), (targetn - 1, sum (IMap.elems mp))])
               else -- Make x the _start_ of the last bin, because the histogram's range is [min, max)
                    let newma = x + (x - mi) / fromIntegral (targetn - 1)
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
  , statsMin' :: !(Maybe' (Min a))
  , statsMax' :: !(Maybe' (Max a))
  , statsHist' :: !(If (StatOpHist op) (RunningHist a) ()) }

statsEmpty :: forall op a. (Num a, KnownStatOptions op) => If (StatOpHist op) Int () -> Stats op a
statsEmpty histsize =
  Stats (mkIf @(Sum a) @() (knownBool @(StatOpMean op)) (Sum 0) ())
        (mkIf @(Sum a) @() (knownBool @(StatOpStddev op)) (Sum 0) ())
        0
        Nothing' Nothing'
        (mkIf @(RunningHist a) @() (knownBool @(StatOpHist op)) (rhistInit histsize) ())

statsAdd :: forall a op.
            (Num a, Ord a, KnownStatOptions op, If (StatOpHist op) (RealFrac a) (() :: Constraint))
         => a -> Stats op a -> Stats op a
statsAdd x s = Stats
  { statsSum = mkIf @(Sum a) @() (knownBool @(StatOpMean op)) (Sum x <> statsSum s) ()
  , statsSumSq = mkIf @(Sum a) @() (knownBool @(StatOpStddev op)) (Sum (x * x) <> statsSumSq s) ()
  , statsCount = 1 <> statsCount s
  , statsMin' = Just' (Min x) <> statsMin' s
  , statsMax' = Just' (Max x) <> statsMax' s
  , statsHist' = mkIf @(RunningHist a) @() (knownBool @(StatOpHist op)) (rhistAdd x (statsHist' s)) () }

statsMean' :: (Fractional b, StatOpMean op ~ True) => (a -> b) -> Stats op a -> b
statsMean' f s = f (getSum (statsSum s)) / fromIntegral (getSum (statsCount s))

statsMean :: (Fractional a, StatOpMean op ~ True) => Stats op a -> a
statsMean = statsMean' id

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

-- | Returns 'Nothing' if dataset is empty.
statsMin :: Stats op a -> Maybe a
statsMin s = case statsMin' s of
               Nothing' -> Nothing
               Just' (Min x) -> Just x

-- | Returns 'Nothing' if dataset is empty.
statsMax :: Stats op a -> Maybe a
statsMax s = case statsMax' s of
               Nothing' -> Nothing
               Just' (Max x) -> Just x

-- | Returns 'Nothing' if there is no data, and hence nothing to base histogram
-- bounds on.
--
-- If there is data, returns lower limit of first bar, upper limit of last bar,
-- and the counts of the bars in between. The bar separators are evenly spaced,
-- and a bar's range is inclusive-exclusive.
statsHist :: StatOpHist op ~ True => Stats op a -> Maybe (a, a, [Int])
statsHist s = rhistFinish (statsHist' s)

prettyStats :: forall op a. (Show a, Floating a, KnownStatOptions op) => Stats op a -> String
prettyStats s =
  (case knownBool @(StatOpMean op) of
     STrue -> "Mean: " ++ show (statsMean s)
              ++ (case knownBool @(StatOpStddev op) of
                    STrue -> " ± " ++ show (statsStddev s)
                    SFalse -> "")
              ++ "\n"
     SFalse -> "")
  ++ "Range: [" ++ show (statsMin s) ++ ", " ++ show (statsMax s) ++ "]\n"
  ++ (case knownBool @(StatOpHist op) of
        STrue
          | Just (mi, ma, l) <- statsHist s ->
              let sMi = show mi
                  prelen = length sMi
                  height = 4
                  maxval = maximum l
                  table = transpose
                    [if val > 0 && val * (height * 8) `quot` maxval == 0
                       then replicate (height - 1) ' ' ++ "."
                       else [" ▁▂▃▄▅▆▇█" !! max 0 (min 8 (val * (height * 8) `quot` maxval - 8 * i))
                            | i <- [height-1, height-2 .. 0]]
                    | val <- l]
              in "Histogram: " ++ show (statsHist' s) ++ "\n"
                 ++ unlines ([replicate prelen ' ' ++ " [" ++ r ++ "]" | r <- init table]
                             ++ [sMi ++ " [" ++ last table ++ "] " ++ show ma])
          | otherwise -> "Histogram: <empty dataset>\n"
        SFalse -> "")
