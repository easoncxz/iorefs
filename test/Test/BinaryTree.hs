module Test.BinaryTree where

import BinaryTree

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Test.QuickCheck
  ( Gen
  , NonEmptyList(NonEmpty)
  , Property
  , (==>)
  , discard
  , elements
  )

prop_foldTree :: BinaryTree Int -> Bool
prop_foldTree t = foldTree Branch Empty t == t

prop_head :: BinaryTree Int -> Bool
prop_head t =
  case t of
    Empty -> BinaryTree.head t == Nothing
    _ -> Just (List.head (inorderTraversal t)) == BinaryTree.head t

prop_last :: BinaryTree Int -> Bool
prop_last t =
  case t of
    Empty -> BinaryTree.last t == Nothing
    _ -> Just (List.last (inorderTraversal t)) == BinaryTree.last t

prop_searchProperty :: BinaryTree Int -> Bool
prop_searchProperty tree =
  let inorder = inorderTraversal tree
   in inorder == List.sort inorder

prop_searchPropertyStepwise :: [Int] -> Bool
prop_searchPropertyStepwise xs =
  let steps = scanr insert Empty xs
   in all prop_searchProperty steps

prop_deleteMaintainsSearchProperty :: [Int] -> Int -> Bool
prop_deleteMaintainsSearchProperty xs x =
  let tree = fromList xs
   in case (x `elem` xs, delete x tree) of
        (False, Nothing) -> True
        (True, Just small) -> prop_searchProperty small
        _ -> False

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

inorderTraversal' :: BinaryTree a -> [a]
inorderTraversal' Empty = []
inorderTraversal' (Branch n l r) =
  inorderTraversal' l ++ [n] ++ inorderTraversal' r

prop_inorderTraversal :: BinaryTree Int -> Bool
prop_inorderTraversal t = inorderTraversal t == inorderTraversal' t

preorderTraversal' :: BinaryTree a -> [a]
preorderTraversal' Empty = []
preorderTraversal' (Branch n l r) =
  [n] ++ preorderTraversal' l ++ preorderTraversal' r

prop_preorderTraversal :: BinaryTree Int -> Bool
prop_preorderTraversal t = preorderTraversal t == preorderTraversal' t
