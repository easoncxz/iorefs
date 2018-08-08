module Test.EuclideanDivision where

import EuclideanDivision

import Test.QuickCheck

divisionProperty :: (Int -> Int -> (Int, Int)) -> Int -> Int -> Property
divisionProperty dm a b =
  b /= 0 ==>
  let (q, r) = dm a b
   in a == q * b + r

prop_divisionPropertyEuclidean :: Int -> Int -> Property
prop_divisionPropertyEuclidean = divisionProperty divModEuclidean

prop_divisionPropertyEuclidean' :: Int -> Int -> Property
prop_divisionPropertyEuclidean' = divisionProperty divModEuclidean'

prop_divisionPropertyTruncated :: Int -> Int -> Property
prop_divisionPropertyTruncated = divisionProperty quotRem

prop_divisionPropertyFloored :: Int -> Int -> Property
prop_divisionPropertyFloored = divisionProperty divMod

prop_nonNegativeModulus :: Int -> Int -> Property
prop_nonNegativeModulus a b =
  b /= 0 ==>
  let (_, r) = divModEuclidean a b
   in 0 <= r && r < abs b

prop_nonNegativeModulus' :: Int -> Int -> Property
prop_nonNegativeModulus' a b =
  b /= 0 ==>
  let (_, r) = divModEuclidean' a b
   in 0 <= r && r < abs b

prop_truncDivModulusSameSignAsDividend :: Int -> Int -> Property
prop_truncDivModulusSameSignAsDividend a b =
  b /= 0 ==>
  let (_, r) = quotRem a b
   in r == 0 || (compare r 0 == compare a 0 && 0 < abs r && abs r < abs b)

prop_floorDivModulusSameSignAsDivisor :: Int -> Int -> Property
prop_floorDivModulusSameSignAsDivisor a b =
  b /= 0 ==>
  let (_, r) = divMod a b
   in r == 0 || (compare r 0 == compare b 0 && 0 < abs r && abs r < abs b)

prop_divModByQuotRemIsDivMod :: Int -> Int -> Property
prop_divModByQuotRemIsDivMod a b = b /= 0 ==> divMod a b == divModByQuotRem a b

prop_quotRemByDivModIsQuotRem :: Int -> Int -> Property
prop_quotRemByDivModIsQuotRem a b = b /= 0 ==> quotRem a b == quotRemByDivMod a b
