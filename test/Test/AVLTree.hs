module Test.AVLTree where

import AVLTree
import BinaryTree (BinaryTree)
import qualified BinaryTree as BT

import Control.Arrow (first, second)
import Data.Maybe (fromMaybe)

prop_withHeightZipVsFold :: BinaryTree Char -> Bool
prop_withHeightZipVsFold t = BT.zipTree t (heightTree t) == treeWithHeight t

prop_insertCommutesWithHeight :: Char -> BinaryTree Char -> Bool
prop_insertCommutesWithHeight x tree =
  treeWithHeight (BT.insert x tree) == insertWithHeight x (treeWithHeight tree)

prop_deleteCommutesWithHeight :: Char -> BinaryTree Char -> Bool
prop_deleteCommutesWithHeight x tree =
  fmap treeWithHeight (BT.delete x tree) == deleteWithHeight x (treeWithHeight tree)

prop_popHeadCommutesWithHeight :: BinaryTree Char -> Bool
prop_popHeadCommutesWithHeight tree =
  fmap (second treeWithHeight) (BT.popHead tree) ==
  fmap (first fst) (popHeadWithHeight (treeWithHeight tree))

prop_popLastCommutesWithHeight :: BinaryTree Char -> Bool
prop_popLastCommutesWithHeight tree =
  fmap (first treeWithHeight) (BT.popLast tree) ==
  fmap (second fst) (popLastWithHeight (treeWithHeight tree))

prop_rotateBackAndForthAgain :: BinaryTree Char -> Bool
prop_rotateBackAndForthAgain t =
  let th = treeWithHeight t
   in fromMaybe th (rotateLeftMaybe =<< rotateRightMaybe th) == th &&
      fromMaybe th (rotateRightMaybe =<< rotateLeftMaybe th) == th

prop_insertWithHeightAVLPreservesAVLProperty :: [Char] -> Bool
prop_insertWithHeightAVLPreservesAVLProperty xs =
  let steps = scanl (flip insertWithHeightAVL) BT.Empty xs
   in all isAVL steps
