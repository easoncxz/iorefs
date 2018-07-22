module Test.AVLTree where

import AVLTree
import BinaryTree (BinaryTree)
import qualified BinaryTree as BT

import Data.Maybe (fromMaybe)

prop_withHeightZipVsFold :: BinaryTree Int -> Bool
prop_withHeightZipVsFold t = BT.zipTree t (heightTree t) == treeWithHeight t

prop_insertWithHeightHomomorphism :: Int -> BinaryTree Int -> Bool
prop_insertWithHeightHomomorphism x tree =
  treeWithHeight (BT.insert x tree) == insertWithHeight x (treeWithHeight tree)

prop_rotateBackAndForthAgain :: BinaryTree Int -> Bool
prop_rotateBackAndForthAgain t =
  let th = treeWithHeight t
   in fromMaybe th (rotateLeftMaybe =<< rotateRightMaybe th) == th &&
      fromMaybe th (rotateRightMaybe =<< rotateLeftMaybe th) == th

prop_insertWithHeightAVLPreservesAVLProperty :: [Int] -> Bool
prop_insertWithHeightAVLPreservesAVLProperty xs =
  let steps = scanl (flip insertWithHeightAVL) BT.Empty xs
   in all isAVL steps
