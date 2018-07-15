{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module QueueTesting where

import BankerQueue (BankerQueue(..))
import qualified BankerQueue as BankerQueue
import FingerQueue (FingerQueue(..))
import qualified FingerQueue
import NaiveLinkedQueue (NaiveLinkedQueue(..))
import qualified NaiveLinkedQueue as NaiveLinkedQueue
import Queue (Queue)
import qualified Queue

import Data.Maybe (fromMaybe)
import Gauge.Main

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f !a = a
applyN !n f !a = applyN (n - 1) f (f a)

mapplyN :: Monad m => Int -> (a -> m a) -> m a -> m a
mapplyN 0 f !a = a
mapplyN !n f !a = mapplyN (n - 1) f (f =<< a)

loadAndEmpty :: Queue q => q Int -> Int -> q Int
loadAndEmpty q n =
  let fullQueue = applyN n (Queue.enqueue 1) q
      emptyQueueM = mapplyN n Queue.dequeue (return fullQueue)
   in fromMaybe Queue.empty emptyQueueM

insertOne :: Queue q => q Char -> Int -> q Char
insertOne q n =
  let large = applyN n (Queue.enqueue '@') q
   in Queue.enqueue '@' large

benchQueue :: Queue q => String -> q Int -> Benchmark
benchQueue name q =
  bgroup
    name
    [bench (show n) (whnf (loadAndEmpty q) (n * 100)) | n <- [1,2 .. 10]]

benchMain :: IO ()
benchMain = do
  defaultMain [benchQueue "NaiveLinkedQueue" NaiveLinkedQueue.empty]
