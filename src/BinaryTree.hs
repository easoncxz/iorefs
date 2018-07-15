{-# LANGUAGE DeriveFunctor #-}

module BinaryTree where

import qualified Data.List as List
import Prelude hiding (head, last, tail)

data BinaryTree a
  = EmptyTree
  | Branch { label :: a
           , left :: BinaryTree a
           , right :: BinaryTree a }
  deriving (Show, Eq, Functor)

leaf :: a -> BinaryTree a
leaf n = Branch n EmptyTree EmptyTree

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
inorderTraversal (Branch n l r) =
  inorderTraversal l ++ [n] ++ inorderTraversal r

preorderTraversal :: BinaryTree a -> [a]
preorderTraversal EmptyTree = []
preorderTraversal (Branch n l r) =
  [n] ++ preorderTraversal l ++ preorderTraversal r

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

prop_labelAndUnlabel :: [Int] -> Bool
prop_labelAndUnlabel xs =
  let tree = fromList xs
      withLabel = labelWithHeight tree
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

zipTree :: BinaryTree a -> BinaryTree b -> BinaryTree (a, b)
zipTree EmptyTree _ = EmptyTree
zipTree _ EmptyTree = EmptyTree
zipTree (Branch a al ar) (Branch b bl br) =
  Branch (a, b) (zipTree al bl) (zipTree ar br)

prop_labelFromZip :: [Int] -> Bool
prop_labelFromZip xs =
  let tree = fromList xs
      labeled = labelWithHeight tree
      zipped = zipTree tree (heightTree tree)
   in labeled == zipped
