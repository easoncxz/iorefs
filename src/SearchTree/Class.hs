module SearchTree.Class where

import SearchTree.BinaryTree (BinaryTree(Branch, Empty))
import qualified SearchTree.BinaryTree as BT
import SearchTree.AVLTree (AVLTree(runAVLTree))
import qualified SearchTree.AVLTree as AVL

class (Foldable t, Functor t) =>
      SearchTree (t :: * -> *)
  where
  empty :: t a
  null :: t a -> Bool
  insert :: Ord a => a -> t a -> t a
  delete :: Ord a => a -> t a -> Maybe (t a)
  head :: t a -> Maybe a
  last :: t a -> Maybe a
  popHead :: t a -> Maybe (a, t a)
  popLast :: t a -> Maybe (t a, a)

instance SearchTree BinaryTree where
  empty = BT.empty
  null = BT.null
  insert = BT.insert
  delete = BT.delete
  head = BT.head
  last = BT.last
  popHead = BT.popHead
  popLast = BT.popLast

instance SearchTree AVLTree where
  empty = AVL.empty
  null = AVL.null
  insert = AVL.insert
  delete = AVL.delete
  head = AVL.head
  last = AVL.last
  popHead = AVL.popHead
  popLast = AVL.popLast
