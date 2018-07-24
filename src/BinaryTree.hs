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

type EmptyCons z = z

type BranchCons a z = a -> z -> z -> z

type TreeAlgebra a z = (EmptyCons z, BranchCons a z)

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

abstractPopHead :: BranchCons a (BinaryTree a) -> BinaryTree a -> Maybe (a, BinaryTree a)
abstractPopHead _ Empty = Nothing
abstractPopHead branch (Branch n l r) =
  (fmap . second) (\l' -> branch n l' r) (abstractPopHead branch l) <|> Just (n, r)

popHead :: BinaryTree a -> Maybe (a, BinaryTree a)
popHead = abstractPopHead Branch

abstractPopLast :: BranchCons a (BinaryTree a) -> BinaryTree a -> Maybe (BinaryTree a, a)
abstractPopLast _ Empty = Nothing
abstractPopLast branch (Branch n l r) =
  (fmap . first) (\r' -> branch n l r') (abstractPopLast branch r) <|> Just (l, n)

popLast :: BinaryTree a -> Maybe (BinaryTree a, a)
popLast = abstractPopLast Branch

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

abstractDelete ::
     (Ord a) => TreeAlgebra a (BinaryTree a) -> a -> BinaryTree a -> Maybe (BinaryTree a)
abstractDelete _ _ Empty = Nothing
abstractDelete (empty, branch) x (Branch n l r) =
  case compare x n of
    LT -> branch <$> pure n <*> abstractDelete (empty, branch) x l <*> pure r
    EQ ->
      let fromR = (\(rMin, r') -> branch rMin l r') <$> abstractPopHead branch r
          fromL = (\(l', lMax) -> branch lMax l' r) <$> abstractPopLast branch l
       in fromR <|> fromL <|> Just Empty
    GT -> Branch n l <$> abstractDelete (empty, branch) x r

delete :: (Ord a) => a -> BinaryTree a -> Maybe (BinaryTree a)
delete = abstractDelete idTreeAlgebra

zipTreeWith :: (a -> b -> c) -> BinaryTree a -> BinaryTree b -> BinaryTree c
zipTreeWith _ Empty _ = Empty
zipTreeWith _ _ Empty = Empty
zipTreeWith f (Branch a al ar) (Branch b bl br) =
  Branch (f a b) (zipTreeWith f al bl) (zipTreeWith f ar br)

zipTree :: BinaryTree a -> BinaryTree b -> BinaryTree (a, b)
zipTree = zipTreeWith (,)

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
