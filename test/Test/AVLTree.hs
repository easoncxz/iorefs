module Test.AVLTree where

import AVLTree
import BinaryTree (BinaryTree)
import qualified BinaryTree as BT

import Control.Arrow (first, second)
import Data.Maybe (fromMaybe)

prop_withHeightZipVsFold :: BinaryTree Char -> Bool
prop_withHeightZipVsFold t = BT.zipTree t (heightTree t) == treeWithHeight t

prop_insertCommutesWithWithHeight :: Char -> BinaryTree Char -> Bool
prop_insertCommutesWithWithHeight x tree =
  treeWithHeight (BT.insert x tree) == insertWithHeight x (treeWithHeight tree)

prop_popHeadCommutesWithWithHeight :: BinaryTree Char -> Bool
prop_popHeadCommutesWithWithHeight tree =
  fmap (second treeWithHeight) (BT.popHead tree) ==
  fmap (first fst) (popHeadWithHeight (treeWithHeight tree))

prop_popLastCommutesWithWithHeight :: BinaryTree Char -> Bool
prop_popLastCommutesWithWithHeight tree =
  fmap (second treeWithHeight) (BT.popLast tree) ==
  fmap (first fst) (popLastWithHeight (treeWithHeight tree))

prop_rotateBackAndForthAgain :: BinaryTree Char -> Bool
prop_rotateBackAndForthAgain t =
  let th = treeWithHeight t
   in fromMaybe th (rotateLeftMaybe =<< rotateRightMaybe th) == th &&
      fromMaybe th (rotateRightMaybe =<< rotateLeftMaybe th) == th

prop_insertWithHeightAVLPreservesAVLProperty :: [Char] -> Bool
prop_insertWithHeightAVLPreservesAVLProperty xs =
  let steps = scanl (flip insertWithHeightAVL) BT.Empty xs
   in all isAVL steps
