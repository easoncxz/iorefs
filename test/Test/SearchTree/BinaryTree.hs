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

prop_foldTreeIdentity :: BinaryTree Int -> Bool
prop_foldTreeIdentity t = foldTree Branch Empty t == t

prop_toListMatchesInorderTraversal :: BinaryTree Int -> Bool
prop_toListMatchesInorderTraversal t = toList t == inorderTraversal t

prop_searchTreeContract :: Property
prop_searchTreeContract = SearchTree.contract (Proxy :: Proxy BinaryTree) (Proxy :: Proxy Int)

prop_abstractInsertImplementsOldInsert :: Char -> BinaryTree Char -> Bool
prop_abstractInsertImplementsOldInsert c t = SearchTree.insert c t == oldInsert c t
  where
    oldInsert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
    oldInsert a Empty = leaf a
    oldInsert a (Branch l n r) =
      if a < n
        then Branch (oldInsert a l) n r
        else Branch l n (oldInsert a r)

searchProperty :: (Ord a) => BinaryTree a -> Bool
searchProperty t = toList t == List.sort (toList t)

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
