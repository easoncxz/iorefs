module BankerQueue where

import Queue (Queue)
import qualified Queue

data BankerQueue a =
  BankerQueue [a]
              Int
              [a]
              Int
  deriving (Show, Eq)

check :: BankerQueue a -> BankerQueue a
check q@(BankerQueue back bc front fc)
  -- if null front
 =
  if fc < bc
    then BankerQueue [] 0 (front ++ reverse back) (fc + bc)
    else q

empty :: BankerQueue a
empty = BankerQueue [] 0 [] 0

isEmpty :: BankerQueue a -> Bool
isEmpty (BankerQueue _ _ [] _) = True
isEmpty _ = False

enqueue :: a -> BankerQueue a -> BankerQueue a
enqueue e (BankerQueue b bc f fc) = check $ BankerQueue (e : b) (bc + 1) f fc

front :: BankerQueue a -> Maybe a
front (BankerQueue _ _ [] _) = Nothing
front (BankerQueue _ _ (f:_) _) = Just f

dequeue :: BankerQueue a -> Maybe (BankerQueue a)
dequeue (BankerQueue _ _ [] _) = Nothing
dequeue (BankerQueue b bc (_:fs) fc) =
  Just (check (BankerQueue b bc fs (fc - 1)))

instance Queue BankerQueue where
  empty = empty
  isEmpty = isEmpty
  enqueue = enqueue
  front = front
  dequeue = dequeue
