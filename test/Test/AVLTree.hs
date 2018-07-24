module Test.AVLTree where

import AVLTree
import BinaryTree (BinaryTree(Branch, Empty))
import qualified BinaryTree as BT

import Test.BinaryTree (searchProperty)

import Control.Arrow (first, second)
import Data.Maybe (fromMaybe)

prop_withHeightZipVsFold :: BinaryTree Char -> Bool
prop_withHeightZipVsFold t = BT.zipTreeWith WithHeight (heightTree t) t == treeWithHeight t

prop_insertCommutesWithHeight :: Char -> BinaryTree Char -> Bool
prop_insertCommutesWithHeight x tree =
  treeWithHeight (BT.insert x tree) == insertWithHeight x (treeWithHeight tree)

prop_deleteCommutesWithHeight :: Char -> BinaryTree Char -> Bool
prop_deleteCommutesWithHeight x tree =
  fmap treeWithHeight (BT.delete x tree) == deleteWithHeight x (treeWithHeight tree)

prop_popHeadCommutesWithHeight :: BinaryTree Char -> Bool
prop_popHeadCommutesWithHeight tree =
  fmap (second treeWithHeight) (BT.popHead tree) ==
  fmap (first whValue) (popHeadWithHeight (treeWithHeight tree))

prop_popHeadWithHeightAVLPreservesAVL :: AVLTree Char -> Bool
prop_popHeadWithHeightAVLPreservesAVL (AVLTree t) =
  case popHeadWithHeightAVL t of
    Nothing -> True
    Just (_, t') -> isAVL (treeWithoutHeight t')

prop_popLastCommutesWithHeight :: BinaryTree Char -> Bool
prop_popLastCommutesWithHeight tree =
  fmap (first treeWithHeight) (BT.popLast tree) ==
  fmap (second whValue) (popLastWithHeight (treeWithHeight tree))

prop_popLastWithHeightAVLPreservesAVL :: AVLTree Char -> Bool
prop_popLastWithHeightAVLPreservesAVL (AVLTree t) =
  case popLastWithHeightAVL t of
    Nothing -> True
    Just (t', _) -> isAVL (treeWithoutHeight t')

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
prop_avlTreesAreAVL (AVLTree t) = isAVL t

prop_insertWithHeightAVLPreservesSearchProperty :: Char -> AVLTree Char -> Bool
prop_insertWithHeightAVLPreservesSearchProperty x (AVLTree t) =
  searchProperty . treeWithoutHeight . insertWithHeightAVL x $ t

prop_insertWithHeightAVLPreservesHeightInvariant :: Char -> AVLTree Char -> Bool
prop_insertWithHeightAVLPreservesHeightInvariant x (AVLTree t) =
  let large = insertWithHeightAVL x t
   in large == treeWithNewHeight large

prop_insertWithHeightAVLPreservesAVLProperty :: Char -> AVLTree Char -> Bool
prop_insertWithHeightAVLPreservesAVLProperty x (AVLTree t) =
  isAVL . treeWithoutHeight . insertWithHeightAVL x $ t

prop_deleteWithHeightAVLPreservesSearchProperty :: Char -> AVLTree Char -> Bool
prop_deleteWithHeightAVLPreservesSearchProperty x (AVLTree t) =
  fromMaybe True . fmap (searchProperty . treeWithoutHeight) $ deleteWithHeightAVL x t

prop_deleteWithHeightAVLPreservesHeightInvariant :: Char -> AVLTree Char -> Bool
prop_deleteWithHeightAVLPreservesHeightInvariant x (AVLTree t) =
  let largeM = deleteWithHeightAVL x t
   in largeM == fmap treeWithNewHeight largeM

prop_deleteWithHeightAVLPreservesAVLProperty :: Char -> AVLTree Char -> Bool
prop_deleteWithHeightAVLPreservesAVLProperty x (AVLTree t) =
  fromMaybe True . fmap (isAVL . treeWithoutHeight) $ deleteWithHeightAVL x t
