
module Test.Bisection where

import Bisection

import qualified Data.List as List
import Data.Maybe (isJust)
import Test.QuickCheck

prop_bisectCanFind :: Int -> [Int] -> Bool
prop_bisectCanFind n xs =
  isJust (bisectFind' n xs) == List.elem n xs
