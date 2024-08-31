{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad
import Data.List (foldl', transpose)
import Data.Maybe (fromJust)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Hedgehog
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property (failDiff)
import GHC.Stack

import qualified PlayHaskellTypes.Statistics.RunningHist as S


genNormal :: RealFloat a => a -> a -> Gen a
genNormal μ σ = do
  -- One half of the Box-Muller transform
  u <- Gen.filter (> 0) $ Gen.realFloat (Range.linearFracFrom 0.6 0 1)
  v <- Gen.realFloat (Range.linearFrac 0 1)
  return (μ + σ * sqrt (-2 * log u) * cos (2 * pi * v))

-- npoints, mean, stddev
data Stage = Stage Int Double Double
  deriving (Show)

-- Maximum double value is 1.80e308
genStage :: Gen Stage
genStage = do
  n <- Gen.integral (Range.linear 1 20)
  μ <- Gen.choice
    [Gen.realFloat (Range.linearFrac (-100) 100)
    ,Gen.realFloat (Range.linearFrac (-1.0e132) (-1.0e130))
    ,Gen.realFloat (Range.linearFrac 1.0e130 1.0e132)]
  σ <- Gen.choice
    [Gen.realFloat (Range.linearFrac 0.5 5)
    ,Gen.realFloat (Range.linearFrac (1.0e-130) (1.0e-128))
    ,Gen.realFloat (Range.linearFrac 1.0e130 1.0e132)]
  return (Stage n μ σ)

genPlan :: Int -> Gen [Stage]
genPlan targetlen = do
  stage@(Stage n μ σ) <- genStage
  if n < targetlen
    then return [Stage targetlen μ σ]
    else (stage :) <$> genPlan (targetlen - n)

runPlan :: [Stage] -> Gen [Double]
runPlan = fmap concat . mapM (\(Stage n μ σ) -> replicateM n (genNormal μ σ))

-- count, mean, stddev, min, max
calcstatsModel :: (Ord a, Floating a) => [a] -> (Int, a, a, a, a)
calcstatsModel l =
  let n = length l
      μ = sum l / fromIntegral n
  in (n, μ, sqrt (1 / (fromIntegral n - 1) * sum [(x-μ) * (x-μ) | x <- l]), minimum l, maximum l)

calcstatsImpl :: (Ord a, Floating a, RealFrac a) => [a] -> ((Int, a, a, a, a), (a, a, [Int]))
calcstatsImpl l =
  let s = foldl' (flip S.statsAdd) (S.statsEmpty @('S.StatOptions True True True) 10) l
  in ((S.statsCount s
      ,S.statsMean s
      ,S.statsStddev s
      ,fromJust (S.statsMin s)
      ,fromJust (S.statsMax s))
     ,fromJust (S.statsHist s))

histogram :: RealFrac a => a -> a -> Int -> [a] -> Maybe [Int]
histogram lo hi nbins l
  -- If the histogram resolution is too small for the float, let's call off this check
  | lo + (hi - lo) / fromIntegral (16 * nbins) == lo = Nothing
  | otherwise =
      Just $ sumEltwise $
        map (\x -> let i | lo == hi = 0
                         | otherwise =
                             min (nbins - 1) . max 0 $
                               floor ((x - lo) / (hi - lo) * fromIntegral nbins)
                   in replicate i 0 ++ [1] ++ replicate (nbins - i - 1) 0)
           l
  where sumEltwise = map sum . transpose

diff' :: (MonadTest m, Show a, Show b, HasCallStack) => String -> a -> (a -> b -> Bool) -> b -> m ()
diff' name x cmp y = do
  ok <- withFrozenCallStack (eval (x `cmp` y))
  if ok
    then success
    else do
      footnote $ "Diff failed: " ++ name
      failDiff x y

checkStats :: (Ord a, Floating a, RealFrac a, Show a) => [a] -> PropertyT IO ()
checkStats l = do
  let (n1, μ1, σ1, lo1, hi1) = calcstatsModel l
      ((n2, μ2, σ2, lo2, hi2), (histlo, histhi, hist)) = calcstatsImpl l
      threshold = 1.0e-5 * maximum (map abs l)
      closeTo a b = abs (a - b) < threshold
  footnote $ "threshold = " ++ show threshold
  diff' "n" n1 (==) n2
  diff' "μ" μ1 closeTo μ2
  diff' "σ" σ1 closeTo σ2
  diff' "lo" lo1 closeTo lo2
  diff' "hi" hi1 closeTo hi2
  diff' "histlo" l (>=) (replicate n1 histlo)
  diff' "histhi" l (<=) (replicate n1 histhi)
  footnote $ "Histogram range: " ++ show (histlo, histhi)
  let fudge = (histhi - histlo) / (fromIntegral (length hist * 8))
  case zip <$> histogram histlo histhi (length hist) (map (subtract fudge) l)
           <*> histogram histlo histhi (length hist) (map (+ fudge) l) of
    Just modelhist ->
      let checkfun left right = and (zipWith (\(a,b) h -> min a b <= h && h <= max a b) left right)
      in diff' "histogram" modelhist checkfun hist
    Nothing -> return ()  -- resolution too small for a sensible histogram

prop_normal :: Property
prop_normal = property $ do
  n <- forAll $ Gen.integral (Range.linear 2 100)
  list <- forAll $ replicateM n (genNormal 7 4)
  checkStats (list :: [Double])

prop_segmented :: Property
prop_segmented = property $ do
  n <- forAll $ Gen.integral (Range.linear 2 100)
  plan <- forAll $ genPlan n
  list <- forAll $ runPlan plan
  checkStats list

main :: IO ()
main = Hedgehog.defaultMain
  [checkParallel $ Group "Statistics.Stats"
    [("normal",) prop_normal
    ,("segmented",) prop_segmented]]
