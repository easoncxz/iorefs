module Test.EuclideanDivision where

import EuclideanDivision (divModEuclidean)

import Test.QuickCheck

divisionProperty :: (Int -> Int -> (Int, Int)) -> Int -> Int -> Property
divisionProperty dm a b =
  b /=
  0 ==>
  let (q, r) = dm a b
   in a == q * b + r

prop_divisionPropertyEuclidean :: Int -> Int -> Property
prop_divisionPropertyEuclidean = divisionProperty divModEuclidean

prop_divisionPropertyTruncated :: Int -> Int -> Property
prop_divisionPropertyTruncated = divisionProperty quotRem

prop_divisionPropertyFloored :: Int -> Int -> Property
prop_divisionPropertyFloored = divisionProperty divMod

prop_nonNegativeModulus :: Int -> Int -> Property
prop_nonNegativeModulus a b =
  b /=
  0 ==>
  let (_, r) = divModEuclidean a b
   in r >= 0

prop_truncDivModulusSameSignAsDividend :: Int -> Int -> Property
prop_truncDivModulusSameSignAsDividend a b =
  b /=
  0 ==>
  let (_, r) = quotRem a b
   in r == 0 || compare r 0 == compare a 0

prop_floorDivModulusSameSignAsDivisor :: Int -> Int -> Property
prop_floorDivModulusSameSignAsDivisor a b =
  b /=
  0 ==>
  let (_, r) = divMod a b
   in r == 0 || compare r 0 == compare b 0
