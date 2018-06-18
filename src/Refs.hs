{-# LANGUAGE ScopedTypeVariables #-}

module Refs where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.IORef
import Data.STRef

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
