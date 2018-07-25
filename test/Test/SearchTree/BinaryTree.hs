module Test.SearchTree.BinaryTree where

import SearchTree.BinaryTree
import qualified SearchTree.Class as SearchTree

import qualified Test.SearchTree as SearchTree

import Control.Arrow (first, second)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Sequence as Seq
import qualified Data.Tuple as Tuple
import Safe (lastMay)
import Test.QuickCheck (Gen, NonEmptyList(NonEmpty), Property, (==>), discard, elements)

prop_foldTree :: BinaryTree Int -> Bool
prop_foldTree t = foldTree Branch Empty t == t

prop_searchTreeContract :: Property
prop_searchTreeContract = SearchTree.contract (Proxy :: Proxy BinaryTree) (Proxy :: Proxy Int)

prop_lastMatchesInorderTraversal :: BinaryTree Int -> Bool
prop_lastMatchesInorderTraversal t = SearchTree.last t == lastMay (inorderTraversal t)

prop_popHeadMatchesInorderTraversal :: BinaryTree Int -> Bool
prop_popHeadMatchesInorderTraversal t =
  fmap (second inorderTraversal) (SearchTree.popHead t) == List.uncons (inorderTraversal t)

prop_popHeadPreservesSearchProperty :: BinaryTree Char -> Bool
prop_popHeadPreservesSearchProperty t =
  case SearchTree.popHead t of
    Nothing -> True
    Just (_, t') -> searchProperty t'

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs =
  case Seq.fromList xs of
    Seq.Empty -> Nothing
    init Seq.:|> last -> Just (toList init, last)

prop_popLastMatchesInorderTraversal :: BinaryTree Int -> Bool
prop_popLastMatchesInorderTraversal tree =
  fmap (first inorderTraversal) (SearchTree.popLast tree) == unsnoc (inorderTraversal tree)

prop_popLastPreservesSearchProperty :: BinaryTree Char -> Bool
prop_popLastPreservesSearchProperty t =
  case SearchTree.popLast t of
    Nothing -> True
    Just (t', _) -> searchProperty t'

searchProperty :: (Ord a) => BinaryTree a -> Bool
searchProperty tree =
  let inorder = inorderTraversal tree
   in inorder == List.sort inorder

prop_searchPropertyStepwise :: [Char] -> Bool
prop_searchPropertyStepwise xs =
  let steps = scanr SearchTree.insert Empty xs
   in all searchProperty steps

prop_elem :: [Int] -> Int -> Bool
prop_elem xs x =
  let tree = SearchTree.fromList xs :: BinaryTree Int
   in SearchTree.elem x tree == Prelude.elem x xs

prop_abstractInsertImplementsOldInsert :: Char -> BinaryTree Char -> Bool
prop_abstractInsertImplementsOldInsert c t = SearchTree.insert c t == oldInsert c t
  where
    oldInsert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
    oldInsert a Empty = leaf a
    oldInsert a (Branch l n r) =
      if a < n
        then Branch (oldInsert a l) n r
        else Branch l n (oldInsert a r)

prop_insertMaintainsSearchProperty :: Char -> BinaryTree Char -> Bool
prop_insertMaintainsSearchProperty x t = searchProperty (SearchTree.insert x t)

prop_deleteMaintainsSearchProperty :: BinaryTree Char -> Char -> Bool
prop_deleteMaintainsSearchProperty tree x =
  case (x `SearchTree.elem` tree, SearchTree.delete x tree) of
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
    Branch l n r -> l `isSubgraphOf` t && r `isSubgraphOf` t

prop_zipUnzipYieldsSubgraphs :: BinaryTree Int -> BinaryTree Char -> Bool
prop_zipUnzipYieldsSubgraphs ti tc =
  let (ti', tc') = unzipTree (zipTree ti tc)
   in ti' `isSubgraphOf` ti && tc' `isSubgraphOf` tc

inorderTraversal' :: BinaryTree a -> [a]
inorderTraversal' Empty = []
inorderTraversal' (Branch l n r) = inorderTraversal' l ++ [n] ++ inorderTraversal' r

prop_inorderTraversal :: BinaryTree Int -> Bool
prop_inorderTraversal t = inorderTraversal t == inorderTraversal' t

preorderTraversal' :: BinaryTree a -> [a]
preorderTraversal' Empty = []
preorderTraversal' (Branch l n r) = [n] ++ preorderTraversal' l ++ preorderTraversal' r

prop_preorderTraversal :: BinaryTree Int -> Bool
prop_preorderTraversal t = preorderTraversal t == preorderTraversal' t

prop_rotateLeftPreservesSearchProperty :: BinaryTree Char -> Bool
prop_rotateLeftPreservesSearchProperty t = searchProperty (rotateLeft t)

prop_rotateRightPreservesSearchProperty :: BinaryTree Char -> Bool
prop_rotateRightPreservesSearchProperty t = searchProperty (rotateRight t)

prop_rotateBackAndForthAgain :: BinaryTree Char -> Bool
prop_rotateBackAndForthAgain t =
  fromMaybe t (rotateLeftMaybe =<< rotateRightMaybe t) == t &&
  fromMaybe t (rotateRightMaybe =<< rotateLeftMaybe t) == t

prop_foldableMatchesInorderTraversal :: BinaryTree Char -> Bool
prop_foldableMatchesInorderTraversal t = toList t == inorderTraversal t
