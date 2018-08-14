module FingerQueue where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

import Queue (Queue)
import qualified Queue

newtype FingerQueue a =
  FingerQueue (Seq a)
  deriving (Show, Eq, Ord)

empty :: FingerQueue a
empty = FingerQueue Seq.empty

isEmpty :: FingerQueue a -> Bool
isEmpty (FingerQueue s) = Seq.null s

enqueue :: a -> FingerQueue a -> FingerQueue a
enqueue e (FingerQueue s) = FingerQueue (e Seq.<| s)

front :: FingerQueue a -> Maybe a
front (FingerQueue s) =
  case s of
    Seq.Empty -> Nothing
    _ Seq.:|> e -> Just e

dequeue :: FingerQueue a -> Maybe (FingerQueue a)
dequeue (FingerQueue s) =
  case s of
    Seq.Empty -> Nothing
    rest Seq.:|> _ -> Just (FingerQueue rest)

instance Queue FingerQueue where
  empty = empty
  isEmpty = isEmpty
  enqueue = enqueue
  front = front
  dequeue = dequeue
