module Test.SearchTree.AVLTree where

import SearchTree.AVLTree
import SearchTree.BinaryTree (BinaryTree(Branch, Empty))
import qualified SearchTree.BinaryTree as BT
import qualified SearchTree.Class as SearchTree

import qualified Test.SearchTree as SearchTree
import Test.SearchTree.BinaryTree (searchProperty)

import Control.Arrow (first, second)
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Proxy

import Test.QuickCheck (Property)

prop_searchTreeProperty :: Property
prop_searchTreeProperty = SearchTree.contract (Proxy :: Proxy AVLTree) (Proxy :: Proxy Int)

prop_withHeightZipVsFold :: BinaryTree Char -> Bool
prop_withHeightZipVsFold t = BT.zipTreeWith WithHeight (heightTree t) t == treeWithHeight t

prop_insertCommutesWithHeight :: Char -> BinaryTree Char -> Bool
prop_insertCommutesWithHeight x tree =
  treeWithHeight (SearchTree.insert x tree) ==
  insertWithHeight (WithHeight undefined x) (treeWithHeight tree)

prop_deleteCommutesWithHeight :: Int -> BinaryTree Int -> Bool
prop_deleteCommutesWithHeight x tree =
  fmap treeWithHeight (SearchTree.delete x tree) ==
  deleteWithHeight (WithHeight undefined x) (treeWithHeight tree)

prop_popHeadCommutesWithHeight :: BinaryTree Char -> Bool
prop_popHeadCommutesWithHeight tree =
  fmap (second treeWithHeight) (SearchTree.popHead tree) ==
  fmap (first whValue) (popHeadWithHeight (treeWithHeight tree))

prop_popHeadPreservesAVL :: AVLTree Char -> Bool
prop_popHeadPreservesAVL t =
  case SearchTree.popHead t of
    Nothing -> True
    Just (_, t') -> validAVL t'

prop_popLastCommutesWithHeight :: BinaryTree Char -> Bool
prop_popLastCommutesWithHeight tree =
  fmap (first treeWithHeight) (SearchTree.popLast tree) ==
  fmap (second whValue) (popLastWithHeight (treeWithHeight tree))

prop_popLastPreservesAVL :: AVLTree Char -> Bool
prop_popLastPreservesAVL t =
  case SearchTree.popLast t of
    Nothing -> True
    Just (t', _) -> validAVL t'

prop_rotateLeftPreservesSearchProperty :: BinaryTree Char -> Bool
prop_rotateLeftPreservesSearchProperty t =
  searchProperty . treeWithoutHeight . rotateLeft . treeWithHeight $ t

prop_rotateRightPreservesSearchProperty :: BinaryTree Char -> Bool
prop_rotateRightPreservesSearchProperty t =
  searchProperty . treeWithoutHeight . rotateRight . treeWithHeight $ t

prop_rotateBackAndForthAgain :: BinaryTree Char -> Bool
prop_rotateBackAndForthAgain t =
  let th = treeWithHeight t
   in fromMaybe th (rotateLeftMaybe =<< rotateRightMaybe th) == th &&
      fromMaybe th (rotateRightMaybe =<< rotateLeftMaybe th) == th

prop_avlTreesAreAVL :: AVLTree Char -> Bool
prop_avlTreesAreAVL = validAVL

prop_insertPreservesSearchProperty :: Char -> AVLTree Char -> Bool
prop_insertPreservesSearchProperty x t =
  searchProperty . treeWithoutHeight . runAVLTree $ SearchTree.insert x t

prop_insertPreservesHeightInvariant :: Char -> AVLTree Char -> Bool
prop_insertPreservesHeightInvariant x t =
  let bigger = runAVLTree (SearchTree.insert x t)
   in bigger == treeWithNewHeight bigger

prop_insertPreservesAVLProperty :: Char -> AVLTree Char -> Bool
prop_insertPreservesAVLProperty x t = validAVL (SearchTree.insert x t)

prop_deletePreservesSearchProperty :: Char -> AVLTree Char -> Bool
prop_deletePreservesSearchProperty x t =
  let smallerM = fmap runAVLTree (SearchTree.delete x t)
   in fromMaybe True . fmap searchProperty $ smallerM

prop_deletePreservesHeightInvariant :: Char -> AVLTree Char -> Bool
prop_deletePreservesHeightInvariant x t =
  let smallerM = fmap runAVLTree (SearchTree.delete x t)
   in smallerM == fmap treeWithNewHeight smallerM

prop_deletePreservesAVLProperty :: Char -> AVLTree Char -> Bool
prop_deletePreservesAVLProperty x t = fromMaybe True . fmap validAVL $ SearchTree.delete x t

prop_foldrMatchesInorderTraversal :: AVLTree Char -> Bool
prop_foldrMatchesInorderTraversal t = toList t == fmap whValue (BT.inorderTraversal (runAVLTree t))
