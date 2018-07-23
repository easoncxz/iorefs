module Test.AVLTree where

import AVLTree
import BinaryTree (BinaryTree)
import qualified BinaryTree as BT

import Control.Arrow (first, second)
import Data.Maybe (fromMaybe)

prop_withHeightZipVsFold :: BinaryTree Char -> Bool
prop_withHeightZipVsFold t = BT.zipTree t (heightTree t) == treeWithHeight t

prop_insertWithHeightPreservesHeightInvariant :: Char -> BinaryTree Char -> Bool
prop_insertWithHeightPreservesHeightInvariant x tree =
  treeWithHeight (BT.insert x tree) == insertWithHeight x (treeWithHeight tree)

prop_deleteWithHeightPreservesHeightInvariant :: Char -> BinaryTree Char -> Bool
prop_deleteWithHeightPreservesHeightInvariant x tree =
  fmap treeWithHeight (BT.delete x tree) == deleteWithHeight x (treeWithHeight tree)

prop_popHeadWithHeightPreservesHeightInvariant :: BinaryTree Char -> Bool
prop_popHeadWithHeightPreservesHeightInvariant tree =
  fmap (second treeWithHeight) (BT.popHead tree) ==
  fmap (first fst) (popHeadWithHeight (treeWithHeight tree))

prop_popLastWithHeightPreservesHeightInvariant :: BinaryTree Char -> Bool
prop_popLastWithHeightPreservesHeightInvariant tree =
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
