module Test.Queues where

import Control.Monad
import qualified Data.List as List
import Data.Proxy
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

queueCorrectness ::
     forall q. Queue q
  => Proxy q
  -> [Int]
  -> Bool
queueCorrectness _ ns =
  let emptyQueue = Queue.empty :: q Int
      fullQueue = foldl (flip Queue.enqueue) emptyQueue ns
   in queueListMatch fullQueue ns

prop_queueCorrectness :: [Int] -> Bool
prop_queueCorrectness ns =
  and
    [ queueCorrectness (Proxy :: Proxy BankerQueue) ns
    , queueCorrectness (Proxy :: Proxy NaiveLinkedQueue) ns
    ]
