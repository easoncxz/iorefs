module Test.AVLTree where

import AVLTree
import BinaryTree (BinaryTree)
import qualified BinaryTree as BT

import Control.Arrow (first, second)
import Data.Maybe (fromMaybe)

prop_withHeightZipVsFold :: BinaryTree Int -> Bool
prop_withHeightZipVsFold t = BT.zipTree t (heightTree t) == treeWithHeight t

prop_insertCommutesWithWithHeight :: Int -> BinaryTree Int -> Bool
prop_insertCommutesWithWithHeight x tree =
  treeWithHeight (BT.insert x tree) == insertWithHeight x (treeWithHeight tree)

prop_popHeadCommutesWithWithHeight :: BinaryTree Int -> Bool
prop_popHeadCommutesWithWithHeight tree =
  (second treeWithHeight <$> BT.popHead tree) ==
  (first snd <$> popHeadWithHeight (treeWithHeight tree))

prop_popLastCommutesWithWithHeight :: BinaryTree Int -> Bool
prop_popLastCommutesWithWithHeight tree =
  (second treeWithHeight <$> BT.popLast tree) ==
  (first snd <$> popHeadWithHeight (treeWithHeight tree))

prop_rotateBackAndForthAgain :: BinaryTree Int -> Bool
prop_rotateBackAndForthAgain t =
  let th = treeWithHeight t
   in fromMaybe th (rotateLeftMaybe =<< rotateRightMaybe th) == th &&
      fromMaybe th (rotateRightMaybe =<< rotateLeftMaybe th) == th

prop_insertWithHeightAVLPreservesAVLProperty :: [Int] -> Bool
prop_insertWithHeightAVLPreservesAVLProperty xs =
  let steps = scanl (flip insertWithHeightAVL) BT.Empty xs
   in all isAVL steps
