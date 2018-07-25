module SearchTree.Class where

import Data.Maybe (isJust)

import Test.QuickCheck (Arbitrary(arbitrary), Gen)

class (Foldable t, Functor t) =>
      SearchTree (t :: * -> *)
  where
  empty :: t a
  null :: t a -> Bool
  insert :: Ord a => a -> t a -> t a
  delete :: Ord a => a -> t a -> Maybe (t a)
  find :: Ord a => a -> t a -> Maybe a
  elem :: Ord a => a -> t a -> Bool
  elem a t = isJust (find a t)
  head :: t a -> Maybe a
  last :: t a -> Maybe a
  popHead :: t a -> Maybe (a, t a)
  popLast :: t a -> Maybe (t a, a)
  fromList :: Ord a => [a] -> t a
  fromList = foldr insert empty
