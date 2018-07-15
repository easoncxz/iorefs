{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Refs where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Foldable
import Data.IORef
import Data.STRef

import Test.QuickCheck

count :: Eq a => a -> [a] -> Int
count n xs =
  runST $ do
    s <- newSTRef 0
    for_ xs $ \x -> when (x == n) (modifySTRef' s (+ 1))
    readSTRef s

prop_count :: Int -> [Int] -> Bool
prop_count n xs = count n xs == length (filter (== n) xs)

wc :: String -> IO Int
wc i = do
  lineCountRef <- newIORef (0 :: Int)
  let ls = lines i
  forM_ ls $ \_ -> do modifyIORef' lineCountRef (+ 1)
  readIORef lineCountRef

swapSTRefs :: STRef s a -> STRef s a -> ST s ()
swapSTRefs ra rb = do
  a <- readSTRef ra
  b <- readSTRef rb
  writeSTRef ra b
  writeSTRef rb a

swapInPair :: (a, a) -> (a, a)
swapInPair (a, b) =
  runST $ do
    ra <- newSTRef a
    rb <- newSTRef b
    swapSTRefs ra rb
    -- (,) <$> readSTRef ra <*> readSTRef rb
    newA <- readSTRef ra
    newB <- readSTRef rb
    return (newA, newB)

prop_swapInPairTwice :: (Int, Int) -> Bool
prop_swapInPairTwice pair = swapInPair (swapInPair pair) == pair

swapElementsInMArray :: (Ix i, MArray a e m) => i -> i -> a i e -> m ()
swapElementsInMArray i j arr = do
  x <- readArray arr i
  y <- readArray arr j
  writeArray arr i y
  writeArray arr j x

-- newArrayFromList :: forall s e. [e] -> ST s (STArray s Int e)
newArrayFromList :: MArray a e m => [e] -> m (a Int e)
newArrayFromList l = newListArray (0, length l - 1) l

prop_roundtripListAndArray :: [Int] -> Bool
prop_roundtripListAndArray xs = runST go == xs
  where
    go :: forall s. ST s [Int]
    go = do
      arr :: STArray s Int Int <- newArrayFromList xs
      getElems arr

swapElementsInList :: forall a. Int -> Int -> [a] -> [a]
swapElementsInList i j xs =
  let go :: forall s. ST s [a]
      go = do
        arr :: STArray s Int a <- newArrayFromList xs
        swapElementsInMArray i j arr
        getElems arr
   in runST go

-- wants better test-case generation
prop_swapElementsInListTwice :: Int -> Int -> [Int] -> Property
prop_swapElementsInListTwice i j xs =
  (0 <= i && i <= j && j < length xs) ==>
  swapElementsInList i j (swapElementsInList i j xs) ==
  xs
