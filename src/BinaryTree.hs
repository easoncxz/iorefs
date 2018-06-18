{-# LANGUAGE DeriveFunctor #-}

module BinaryTree where

import Prelude hiding (head, last, tail)

data BinaryTree a
  = EmptyTree
  | Branch a
           (BinaryTree a)
           (BinaryTree a)
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
