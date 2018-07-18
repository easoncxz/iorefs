{-# LANGUAGE DeriveFunctor #-}

module BinaryTree where

import qualified Data.List as List
import Prelude hiding (head, last, tail)

import Test.QuickCheck

data BinaryTree a
  = EmptyTree
  | Branch { label :: a
           , left :: BinaryTree a
           , right :: BinaryTree a }
  deriving (Show, Eq, Functor)

leaf :: a -> BinaryTree a
leaf n = Branch n EmptyTree EmptyTree

-- | Build a bigger tree with the given subtree as the left-child of a new node
-- with the given value.
asLeft :: BinaryTree a -> a -> BinaryTree a
asLeft t a = Branch a t EmptyTree

asRight :: BinaryTree a -> a -> BinaryTree a
asRight t a = Branch a EmptyTree t

showTree :: Show a => Int -> BinaryTree a -> String
showTree indent t =
  let prefix = replicate indent '\t'
   in case t of
        EmptyTree -> prefix ++ "."
        Branch n left right ->
          concat
            [ prefix ++ show n ++ "\n"
            , showTree (indent + 1) left ++ "\n"
            , showTree (indent + 1) right ++ "\n"
            ]

putTreeLn :: Show a => BinaryTree a -> IO ()
putTreeLn t = putStrLn (showTree 0 t)

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

depth :: BinaryTree a -> Int
depth EmptyTree = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

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

readHeightLabel :: BinaryTree (a, Int) -> Int
readHeightLabel EmptyTree = -1
readHeightLabel (Branch (_, h) _ _) = h

labelWithHeight :: BinaryTree a -> BinaryTree (a, Int)
labelWithHeight EmptyTree = EmptyTree
labelWithHeight (Branch n l r) =
  let lh = labelWithHeight l
      rh = labelWithHeight r
   in Branch (n, 1 + max (readHeightLabel lh) (readHeightLabel rh)) lh rh

removeLabel :: BinaryTree (a, h) -> BinaryTree a
removeLabel EmptyTree = EmptyTree
removeLabel (Branch (n, _) l r) = Branch n (removeLabel l) (removeLabel r)

prop_labelAndUnlabel :: BinaryTree Int -> Bool
prop_labelAndUnlabel tree =
  let withLabel = labelWithHeight tree
      withoutLabel = removeLabel withLabel
   in withoutLabel == tree

nodeHeight :: BinaryTree Int -> Int
nodeHeight EmptyTree = -1
nodeHeight (Branch n _ _) = n

heightTree :: BinaryTree a -> BinaryTree Int
heightTree EmptyTree = EmptyTree
heightTree (Branch n l r) =
  let lt = heightTree l
      rt = heightTree r
   in Branch (1 + max (nodeHeight lt) (nodeHeight rt)) lt rt

heightTree' :: BinaryTree a -> BinaryTree Int
heightTree' EmptyTree = EmptyTree
heightTree' (Branch _ EmptyTree EmptyTree) = leaf 0
heightTree' (Branch _ l EmptyTree) =
  let lt = heightTree' l
      h = nodeHeight lt
   in Branch (h + 1) lt EmptyTree
heightTree' (Branch _ EmptyTree r) =
  let rt = heightTree' r
      h = nodeHeight rt
   in Branch (h + 1) EmptyTree rt
heightTree' (Branch _ l r) =
  let lt = heightTree' l
      rt = heightTree' r
      h = max (nodeHeight lt) (nodeHeight rt)
   in Branch (h + 1) lt rt

prop_heightTree :: BinaryTree Int -> Bool
prop_heightTree tree = heightTree tree == heightTree' tree

zipTree :: BinaryTree a -> BinaryTree b -> BinaryTree (a, b)
zipTree EmptyTree _ = EmptyTree
zipTree _ EmptyTree = EmptyTree
zipTree (Branch a al ar) (Branch b bl br) = Branch (a, b) (zipTree al bl) (zipTree ar br)

-- | Our two ways of getting a `BinaryTree (a, Int)` are equivalent
prop_labelFromZip :: BinaryTree Int -> Bool
prop_labelFromZip tree = labelWithHeight tree == zipTree tree (heightTree tree)

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
balanceFactor (Branch _ l r) = nodeHeight (snd <$> l) - nodeHeight (snd <$> r)

isAVL :: (Ord a) => BinaryTree a -> Bool
isAVL t = go (labelWithHeight t)
  where
    go EmptyTree = True
    go b@(Branch _ l r) = balanceFactor b `elem` [-1 .. 1] && go l && go r
