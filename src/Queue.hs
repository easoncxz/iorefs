module Queue where

class Queue q where
  empty :: q e
  isEmpty :: q e -> Bool
  enqueue :: e -> q e -> q e
  front :: q e -> Maybe e
  dequeue :: q e -> Maybe (q e)
