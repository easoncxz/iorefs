module Test.QuickSelect where

import QuickSelect

import Safe (headMay)
import Data.Maybe (fromMaybe)
import qualified Data.List as List

prop_partition :: [Int] -> Bool
prop_partition xs =
  let nonEmpty = do
        (pivotIx, after) <- partition' xs
        let (smalls, nonsmalls) = List.splitAt pivotIx after
        pivot <- headMay nonsmalls
        return (all (< pivot) smalls && all (>= pivot) nonsmalls)
   in fromMaybe True nonEmpty

prop_selectSmall :: Int -> [Int] -> Bool
prop_selectSmall n xs = selectSmall n xs == modelSelectSmall n xs

prop_quicksort :: [Int] -> Bool
prop_quicksort xs = quicksort xs == List.sort xs
