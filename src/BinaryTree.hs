{-# LANGUAGE DeriveFunctor #-}

module BinaryTree where

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Prelude hiding (head, last, tail)

import Test.QuickCheck (Arbitrary(arbitrary), Property, (==>), discard)

data BinaryTree a
  = EmptyTree
  | Branch { label :: a
           , left :: BinaryTree a
           , right :: BinaryTree a }
  deriving (Show, Eq, Functor)

leaf :: a -> BinaryTree a
leaf n = Branch n EmptyTree EmptyTree

asLeftOf :: BinaryTree a -> a -> BinaryTree a
asLeftOf t a = Branch a t EmptyTree

asRightOf :: BinaryTree a -> a -> BinaryTree a
asRightOf t a = Branch a EmptyTree t

readLabel :: BinaryTree a -> Maybe a
readLabel EmptyTree = Nothing
readLabel (Branch n _ _) = Just n

tree1 :: BinaryTree Int
tree1 = Branch 5 (Branch 2 (leaf 1) (leaf 4)) (leaf 3)

inorderTraversal :: BinaryTree a -> [a]
inorderTraversal EmptyTree = []
inorderTraversal (Branch n l r) = inorderTraversal l ++ [n] ++ inorderTraversal r

preorderTraversal :: BinaryTree a -> [a]
preorderTraversal EmptyTree = []
preorderTraversal (Branch n l r) = [n] ++ preorderTraversal l ++ preorderTraversal r

-- TODO
levelOrderTraversal :: BinaryTree a -> [a]
levelOrderTraversal _ = []

head :: BinaryTree a -> Maybe a
head EmptyTree = Nothing
head (Branch n EmptyTree _) = Just n
head (Branch _ l _) = head l

last :: BinaryTree a -> Maybe a
last EmptyTree = Nothing
last (Branch n _ EmptyTree) = Just n
last (Branch _ _ r) = last r

rootPrev :: BinaryTree a -> Maybe a
rootPrev EmptyTree = Nothing
rootPrev (Branch _ EmptyTree _) = Nothing
rootPrev (Branch _ l _) = last l

rootNext :: BinaryTree a -> Maybe a
rootNext EmptyTree = Nothing
rootNext (Branch _ _ EmptyTree) = Nothing
rootNext (Branch _ _ r) = head r

insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert a EmptyTree = leaf a
insert a b@Branch {label, left, right} =
  if a < label
    then b {left = insert a left}
    else b {right = insert a right}

fromList :: (Ord a) => [a] -> BinaryTree a
fromList = List.foldl' (flip insert) EmptyTree

instance (Arbitrary a, Ord a) => Arbitrary (BinaryTree a) where
  arbitrary = fromList <$> arbitrary

prop_binarySearchTreeInvariant :: [Int] -> Bool
prop_binarySearchTreeInvariant xs =
  let tree = fromList xs
      inorder = inorderTraversal tree
      sorted = List.sort xs
   in inorder == sorted

zipTree :: BinaryTree a -> BinaryTree b -> BinaryTree (a, b)
zipTree EmptyTree _ = EmptyTree
zipTree _ EmptyTree = EmptyTree
zipTree (Branch a al ar) (Branch b bl br) = Branch (a, b) (zipTree al bl) (zipTree ar br)

unzipTree :: BinaryTree (a, b) -> (BinaryTree a, BinaryTree b)
unzipTree EmptyTree = (EmptyTree, EmptyTree)
unzipTree (Branch (a, b) l r) =
  let (la, lb) = unzipTree l
      (ra, rb) = unzipTree r
   in (Branch a la ra, Branch b lb rb)

prop_zipUnzip :: [Int] -> Bool
prop_zipUnzip xs =
  let tree = fromList xs
      heights = heightTree tree
   in unzipTree (zipTree tree heights) == (tree, heights)

-- | Leaf nodes are of height 0
heightTree :: BinaryTree a -> BinaryTree Int
heightTree EmptyTree = EmptyTree
heightTree (Branch n l r) =
  let lt = heightTree l
      rt = heightTree r
      maybeOneMore = do
        tallerChild <- max (readLabel lt) (readLabel rt)
        return (tallerChild + 1)
   in Branch (fromMaybe leafNodeHeight maybeOneMore) lt rt
  where
    leafNodeHeight = 0

withHeight :: BinaryTree t -> BinaryTree (t, Int)
withHeight t = zipTree t (heightTree t)

height :: BinaryTree t -> Maybe Int
height = fmap snd . readLabel . withHeight

isSubtreeOf :: (Eq a) => BinaryTree a -> BinaryTree a -> Bool
isSubtreeOf EmptyTree _ = True
isSubtreeOf Branch {} EmptyTree = False
isSubtreeOf a@(Branch an al ar) b@(Branch bn bl br) =
  an == bn && al `isSubtreeOf` bl && ar `isSubtreeOf` br || a `isSubtreeOf` bl || a `isSubtreeOf` br

prop_subtree :: BinaryTree Int -> Bool
prop_subtree t =
  case t of
    EmptyTree -> discard
    Branch n l r -> l `isSubtreeOf` t && r `isSubtreeOf` t

prop_subtreeTransitivity :: BinaryTree Int -> BinaryTree Int -> BinaryTree Int -> Property
prop_subtreeTransitivity a b c = a `isSubtreeOf` b && b `isSubtreeOf` c ==> a `isSubtreeOf` c

prop_zipYieldsSubtrees :: BinaryTree Int -> BinaryTree Char -> Bool
prop_zipYieldsSubtrees ta tb =
  let (ta', tb') = unzipTree (zipTree ta tb)
   in ta' `isSubtreeOf` ta && tb' `isSubtreeOf` tb

-- | Positive corresponds to a taller left-subtree
balanceFactor :: BinaryTree (a, Int) -> Int
balanceFactor EmptyTree = 0
balanceFactor (Branch _ l r) = readHeight l - readHeight r
  where
    readHeight EmptyTree = -1
    readHeight (Branch (_, h) _ _) = h

isAVL :: (Ord a) => BinaryTree a -> Bool
isAVL t = go (withHeight t)
  where
    go EmptyTree = True
    go b@(Branch _ l r) = balanceFactor b `elem` [-1 .. 1] && go l && go r
