module TestQueues where

import Control.Monad
import qualified Data.List as List
import Test.QuickCheck

import BankerQueue (BankerQueue(..))
import qualified BankerQueue
import FingerQueue (FingerQueue(..))
import qualified FingerQueue
import NaiveLinkedQueue (NaiveLinkedQueue(..))
import qualified NaiveLinkedQueue
import Queue (Queue)
import qualified Queue

pop :: Queue q => q e -> Maybe (e, q e)
pop q =
  case (Queue.dequeue q, Queue.front q) of
    (Just q', Just e) -> Just (e, q')
    _ -> Nothing

queueListMatch :: (Eq e, Queue q) => q e -> [e] -> Bool
queueListMatch q l =
  case (pop q, List.uncons l) of
    (Just (qe, qr), Just (le, lr))
      | qe == le -> queueListMatch qr lr
    (Nothing, Nothing) -> True
    _ -> False

prop_queueCorrectness :: Queue q => q Int -> [Int] -> Bool
prop_queueCorrectness q ns =
  let fullQueue = foldl (flip Queue.enqueue) q ns
   in queueListMatch fullQueue ns
