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

prop_zipUnzip :: BinaryTree Int -> BinaryTree Int -> Bool
prop_zipUnzip x y =
  let zipped = zipTree x y
   in uncurry zipTree (unzipTree zipped) == zipped

prop_subtreesAreSubgraphs :: BinaryTree Int -> Bool
prop_subtreesAreSubgraphs t =
  case t of
    Empty -> discard
    Branch n l r -> l `isSubgraphOf` t && r `isSubgraphOf` t

prop_zipUnzipYieldsSubgraphs :: BinaryTree Int -> BinaryTree Char -> Bool
prop_zipUnzipYieldsSubgraphs ti tc =
  let (ti', tc') = unzipTree (zipTree ti tc)
   in ti' `isSubgraphOf` ti && tc' `isSubgraphOf` tc
