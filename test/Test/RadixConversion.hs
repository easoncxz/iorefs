module Test.RadixConversion where

import RadixConversion

import Numeric (showHex, showOct)
import Test.QuickCheck (NonNegative(..), Positive(..), Property, (==>))

prop_arbitraryBaseIsGood :: Base -> Bool
prop_arbitraryBaseIsGood b = not (getBase b `elem` [-1 .. 1])

prop_interpretMatchesShow :: Int -> Bool
prop_interpretMatchesShow n = interpretWithSign (unsafeBase 10) (show n) == Just n

prop_roundtrip :: Base -> Int -> Bool
prop_roundtrip b n = interpretWithSign b (representWithSign b n) == Just n

prop_zeroIsZeroRegardlessOfBase :: Base -> Bool
prop_zeroIsZeroRegardlessOfBase b = representWithSign b 0 == "0"

prop_representingNegativeNumbersInPositiveBaseUsingNegativeSign ::
     Positive Base -> Positive Int -> Bool
prop_representingNegativeNumbersInPositiveBaseUsingNegativeSign (Positive b) (Positive n) =
  representWithSign b (-n) == '-' : representWithSign b n

prop_representWithSignMatchesNumeric :: NonNegative Int -> Bool
prop_representWithSignMatchesNumeric (NonNegative n) =
  representWithSign (unsafeBase 8) n == showOct n "" &&
  representWithSign (unsafeBase 16) n == showHex n ""
