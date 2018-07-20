module Test.BinaryTree where

import BinaryTree

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Test.QuickCheck (discard)

prop_searchProperty :: [Int] -> Bool
prop_searchProperty xs =
  let tree = fromList xs
      inorder = inorderTraversal tree
      sorted = List.sort xs
   in inorder == sorted

prop_zipUnzip :: [Int] -> Bool
prop_zipUnzip xs =
  let tree = fromList xs
      heights = heightTree tree
   in unzipTree (zipTree tree heights) == (tree, heights)

prop_insertWithHeightHomomorphism :: Int -> BinaryTree Int -> Bool
prop_insertWithHeightHomomorphism x tree =
  withHeight (insert x tree) == insertWithHeight x (withHeight tree)

prop_subtree :: BinaryTree Int -> Bool
prop_subtree t =
  case t of
    EmptyTree -> discard
    Branch (Node n l r) -> l `isSubtreeOf` t && r `isSubtreeOf` t

prop_zipYieldsSubtrees :: BinaryTree Int -> BinaryTree Char -> Bool
prop_zipYieldsSubtrees ta tb =
  let (ta', tb') = unzipTree (zipTree ta tb)
   in ta' `isSubtreeOf` ta && tb' `isSubtreeOf` tb

prop_rotateBackAndForthAgain :: BinaryTree Int -> Bool
prop_rotateBackAndForthAgain t =
  let th = withHeight t
   in fromMaybe th (rotateLeftMaybe =<< rotateRightMaybe th) == th &&
      fromMaybe th (rotateRightMaybe =<< rotateLeftMaybe th) == th
