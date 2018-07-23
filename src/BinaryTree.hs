module BinaryTree where

import Control.Applicative
import Control.Arrow (first, second)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Prelude hiding (elem, head, last, tail)

import Test.QuickCheck (Arbitrary(arbitrary), Property, (==>), conjoin, discard)

data BinaryTree a
  = Empty
  | Branch a
           (BinaryTree a)
           (BinaryTree a)
  deriving (Show, Eq, Functor)

null :: BinaryTree a -> Bool
null Empty = True
null (Branch _ _ _) = False

leaf :: a -> BinaryTree a
leaf n = Branch n Empty Empty

asLeftOf :: BinaryTree a -> a -> BinaryTree a
asLeftOf t a = Branch a t Empty

asRightOf :: BinaryTree a -> a -> BinaryTree a
asRightOf t a = Branch a Empty t

node :: BinaryTree a -> Maybe a
node Empty = Nothing
node (Branch n _ _) = Just n

type TreeAlgebra a z = (z, a -> z -> z -> z)

type IdTreeAlgebra a = TreeAlgebra a (BinaryTree a)

idTreeAlgebra :: IdTreeAlgebra a
idTreeAlgebra = (Empty, Branch)

foldTree :: (a -> z -> z -> z) -> z -> BinaryTree a -> z
foldTree _ z Empty = z
foldTree f z (Branch n l r) = f n (foldTree f z l) (foldTree f z r)

inorderTraversal :: BinaryTree a -> [a]
inorderTraversal = foldTree inorder []
  where
    inorder :: a -> [a] -> [a] -> [a]
    inorder n ls rs = ls ++ [n] ++ rs

preorderTraversal :: BinaryTree a -> [a]
preorderTraversal = foldTree preorder []
  where
    preorder :: a -> [a] -> [a] -> [a]
    preorder n ls rs = [n] ++ ls ++ rs

head :: BinaryTree a -> Maybe a
head = foldTree leftOrSelf Nothing
  where
    leftOrSelf :: a -> Maybe a -> Maybe a -> Maybe a
    leftOrSelf n l _ = l <|> Just n

last :: BinaryTree a -> Maybe a
last = foldTree rightOrSelf Nothing
  where
    rightOrSelf :: a -> Maybe a -> Maybe a -> Maybe a
    rightOrSelf n _ r = r <|> Just n

popHead :: BinaryTree a -> Maybe (a, BinaryTree a)
popHead Empty = Nothing
popHead (Branch n l r) = (fmap . second) (\l' -> Branch n l' r) (popHead l) <|> Just (n, r)

popLast :: BinaryTree a -> Maybe (BinaryTree a, a)
popLast Empty = Nothing
popLast (Branch n l r) = (fmap . first) (\r' -> Branch n l r') (popLast r) <|> Just (l, n)

rootPrev :: BinaryTree a -> Maybe a
rootPrev Empty = Nothing
rootPrev (Branch _ Empty _) = Nothing
rootPrev (Branch _ l _) = last l

rootNext :: BinaryTree a -> Maybe a
rootNext Empty = Nothing
rootNext (Branch _ _ Empty) = Nothing
rootNext (Branch _ _ r) = head r

abstractInsert :: (Ord a) => TreeAlgebra a z -> a -> BinaryTree a -> z
abstractInsert (empty, branch) x Empty = branch x empty empty
abstractInsert (empty, branch) x (Branch n l r) =
  if x < n
    then branch n (abstractInsert (empty, branch) x l) (foldTree branch empty r)
    else branch n (foldTree branch empty l) (abstractInsert (empty, branch) x r)

insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert = abstractInsert idTreeAlgebra

fromList :: (Ord a) => [a] -> BinaryTree a
fromList = List.foldl' (flip insert) Empty

instance (Arbitrary a, Ord a) => Arbitrary (BinaryTree a) where
  arbitrary = fromList <$> arbitrary

elem :: (Ord a) => a -> BinaryTree a -> Bool
elem _ Empty = False
elem x (Branch n l r) =
  case compare x n of
    LT -> x `elem` l
    EQ -> True
    GT -> x `elem` r

delete :: (Ord a) => a -> BinaryTree a -> Maybe (BinaryTree a)
delete n Empty = Nothing
delete x (Branch n l r) =
  case compare x n of
    LT -> Branch <$> pure n <*> delete x l <*> pure r
    EQ ->
      let deleteR = (\(rMin, r') -> Branch rMin l r') <$> popHead r
          deleteL = (\(l', lMax) -> Branch lMax l' r) <$> popLast l
       in deleteR <|> deleteL <|> Just Empty
    GT -> Branch n l <$> delete x r

zipTree :: BinaryTree a -> BinaryTree b -> BinaryTree (a, b)
zipTree Empty _ = Empty
zipTree _ Empty = Empty
zipTree (Branch a al ar) (Branch b bl br) = Branch (a, b) (zipTree al bl) (zipTree ar br)

unzipTree :: BinaryTree (a, b) -> (BinaryTree a, BinaryTree b)
unzipTree Empty = (Empty, Empty)
unzipTree (Branch (a, b) l r) =
  let (la, lb) = unzipTree l
      (ra, rb) = unzipTree r
   in (Branch a la ra, Branch b lb rb)

isSubgraphOf :: (Eq a) => BinaryTree a -> BinaryTree a -> Bool
isSubgraphOf Empty _ = True
isSubgraphOf (Branch _ _ _) Empty = False
isSubgraphOf a@(Branch an al ar) b@(Branch bn bl br) =
  an == bn && al `isSubgraphOf` bl && ar `isSubgraphOf` br ||
  a `isSubgraphOf` bl || a `isSubgraphOf` br
