module Test.BinaryTree where

import BinaryTree

import Control.Arrow (first, second)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Tuple as Tuple
import Safe (headMay, lastMay)
import Test.QuickCheck (Gen, NonEmptyList(NonEmpty), Property, (==>), discard, elements)

prop_foldTree :: BinaryTree Int -> Bool
prop_foldTree t = foldTree Branch Empty t == t

prop_headMatchesInorderTraversal :: BinaryTree Int -> Bool
prop_headMatchesInorderTraversal t = BinaryTree.head t == headMay (inorderTraversal t)

prop_lastMatchesInorderTraversal :: BinaryTree Int -> Bool
prop_lastMatchesInorderTraversal t = BinaryTree.last t == lastMay (inorderTraversal t)

prop_popHeadMatchesInorderTraversal :: BinaryTree Int -> Bool
prop_popHeadMatchesInorderTraversal t =
  fmap (second inorderTraversal) (popHead t) == List.uncons (inorderTraversal t)

prop_popHeadPreservesSearchProperty :: BinaryTree Char -> Bool
prop_popHeadPreservesSearchProperty t =
  case popHead t of
    Nothing -> True
    Just (_, t') -> searchProperty t'

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs =
  case Seq.fromList xs of
    Seq.Empty -> Nothing
    init Seq.:|> last -> Just (toList init, last)

prop_popLastMatchesInorderTraversal :: BinaryTree Int -> Bool
prop_popLastMatchesInorderTraversal tree =
  fmap (first inorderTraversal) (popLast tree) == unsnoc (inorderTraversal tree)

prop_popLastPreservesSearchProperty :: BinaryTree Char -> Bool
prop_popLastPreservesSearchProperty t =
  case popLast t of
    Nothing -> True
    Just (t', _) -> searchProperty t'

searchProperty :: (Ord a) => BinaryTree a -> Bool
searchProperty tree =
  let inorder = inorderTraversal tree
   in inorder == List.sort inorder

prop_searchPropertyStepwise :: [Char] -> Bool
prop_searchPropertyStepwise xs =
  let steps = scanr insert Empty xs
   in all searchProperty steps

prop_elem :: [Int] -> Int -> Bool
prop_elem xs x =
  let tree = fromList xs
   in BinaryTree.elem x tree == Prelude.elem x xs

prop_deleteMaintainsSearchProperty :: BinaryTree Char -> Char -> Bool
prop_deleteMaintainsSearchProperty tree x =
  case (x `BinaryTree.elem` tree, delete x tree) of
    (False, Nothing) -> True
    (True, Just small) -> searchProperty small
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
inorderTraversal' (Branch n l r) = inorderTraversal' l ++ [n] ++ inorderTraversal' r

prop_inorderTraversal :: BinaryTree Int -> Bool
prop_inorderTraversal t = inorderTraversal t == inorderTraversal' t

preorderTraversal' :: BinaryTree a -> [a]
preorderTraversal' Empty = []
preorderTraversal' (Branch n l r) = [n] ++ preorderTraversal' l ++ preorderTraversal' r

prop_preorderTraversal :: BinaryTree Int -> Bool
prop_preorderTraversal t = preorderTraversal t == preorderTraversal' t
