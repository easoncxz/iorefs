module Test.Refs where

import Refs

import Control.Monad.ST
import Data.Array.ST
import Test.QuickCheck (Property, (==>), elements)

prop_count :: Int -> [Int] -> Bool
prop_count n xs = count n xs == length (filter (== n) xs)

prop_swapInPairTwice :: (Int, Int) -> Bool
prop_swapInPairTwice pair = swapInPair (swapInPair pair) == pair

prop_roundtripListAndArray :: [Int] -> Bool
prop_roundtripListAndArray xs = runST go == xs
  where
    go :: forall s. ST s [Int]
    go = do
      arr :: STArray s Int Int <- newArrayFromList xs
      getElems arr

prop_swapElementsInListTwice :: [Int] -> Property
prop_swapElementsInListTwice xs =
  not (null xs) ==> do
    i <- elements [0 .. length xs - 1]
    j <- elements [0 .. length xs - 1]
    return $ swapElementsInList i j (swapElementsInList i j xs) == xs
