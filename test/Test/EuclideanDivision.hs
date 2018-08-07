module Test.EuclideanDivision where

import EuclideanDivision (divModEuclidean)

import Test.QuickCheck

prop_divisionProperty :: Int -> Int -> Property
prop_divisionProperty a b =
  b /=
  0 ==>
  let (q, r) = divModEuclidean a b
   in a == q * b + r

prop_nonNegativeModulus :: Int -> Int -> Property
prop_nonNegativeModulus a b =
  b /=
  0 ==>
  let (_, r) = divModEuclidean a b
   in r >= 0
