module NaiveLinkedQueue where

import Queue (Queue)
import qualified Queue

newtype NaiveLinkedQueue a =
  NaiveLinkedQueue [a]

empty :: NaiveLinkedQueue a
empty = NaiveLinkedQueue []

isEmpty :: NaiveLinkedQueue a -> Bool
isEmpty (NaiveLinkedQueue []) = True
isEmpty _ = False

enqueue :: a -> NaiveLinkedQueue a -> NaiveLinkedQueue a
enqueue e (NaiveLinkedQueue xs) = NaiveLinkedQueue (xs ++ [e])

front :: NaiveLinkedQueue a -> Maybe a
front (NaiveLinkedQueue []) = Nothing
front (NaiveLinkedQueue (e:_)) = Just e

dequeue :: NaiveLinkedQueue a -> Maybe (NaiveLinkedQueue a)
dequeue (NaiveLinkedQueue []) = Nothing
dequeue (NaiveLinkedQueue (_:xs)) = Just (NaiveLinkedQueue xs)

instance Queue NaiveLinkedQueue where
  empty = empty
  isEmpty = isEmpty
  enqueue = enqueue
  front = front
  dequeue = dequeue
