module SearchTree.BinaryTree where

import SearchTree.Class (SearchTree(..))

import Control.Applicative
import Control.Arrow (first, second)
import Control.DeepSeq (NFData)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Prelude hiding (head, last, tail)

import Test.QuickCheck (Arbitrary(arbitrary), Property, (==>), conjoin, discard)

data BinaryTree a
  = Empty
  | Branch (BinaryTree a)
           a
           (BinaryTree a)
  deriving (Show, Eq, Functor, Foldable, Generic)

instance NFData a => NFData (BinaryTree a)

leaf :: a -> BinaryTree a
leaf n = Branch Empty n Empty

asLeftOf :: BinaryTree a -> a -> BinaryTree a
asLeftOf t a = Branch t a Empty

asRightOf :: BinaryTree a -> a -> BinaryTree a
asRightOf t a = Branch Empty a t

node :: BinaryTree a -> Maybe a
node Empty = Nothing
node (Branch _ n _) = Just n

type EmptyCons z = z

type BranchCons a z = z -> a -> z -> z

type TreeAlgebra a z = (EmptyCons z, BranchCons a z)

type IdTreeAlgebra a = TreeAlgebra a (BinaryTree a)

idTreeAlgebra :: IdTreeAlgebra a
idTreeAlgebra = (Empty, Branch)

foldTree :: (z -> a -> z -> z) -> z -> BinaryTree a -> z
foldTree _ z Empty = z
foldTree f z (Branch l n r) = f (foldTree f z l) n (foldTree f z r)

inorderTraversal :: BinaryTree a -> [a]
inorderTraversal = foldTree inorder []
  where
    inorder :: [a] -> a -> [a] -> [a]
    inorder ls n rs = ls ++ [n] ++ rs

preorderTraversal :: BinaryTree a -> [a]
preorderTraversal = foldTree preorder []
  where
    preorder :: [a] -> a -> [a] -> [a]
    preorder ls n rs = [n] ++ ls ++ rs

abstractPopHead :: BranchCons a (BinaryTree a) -> BinaryTree a -> Maybe (a, BinaryTree a)
abstractPopHead _ Empty = Nothing
abstractPopHead branch (Branch l n r) =
  (fmap . second) (\l' -> branch l' n r) (abstractPopHead branch l) <|> Just (n, r)

abstractPopLast :: BranchCons a (BinaryTree a) -> BinaryTree a -> Maybe (BinaryTree a, a)
abstractPopLast _ Empty = Nothing
abstractPopLast branch (Branch l n r) =
  (fmap . first) (\r' -> branch l n r') (abstractPopLast branch r) <|> Just (l, n)

rootPrev :: BinaryTree a -> Maybe a
rootPrev Empty = Nothing
rootPrev (Branch Empty _ _) = Nothing
rootPrev (Branch l _ _) = last l

rootNext :: BinaryTree a -> Maybe a
rootNext Empty = Nothing
rootNext (Branch _ _ Empty) = Nothing
rootNext (Branch _ _ r) = head r

abstractInsert :: (Ord a) => BranchCons a (BinaryTree a) -> a -> BinaryTree a -> BinaryTree a
abstractInsert branch x Empty = branch Empty x Empty
abstractInsert branch x (Branch l n r) =
  if x < n
    then branch (abstractInsert branch x l) n r
    else branch l n (abstractInsert branch x r)

findImpl :: (Ord a) => a -> BinaryTree a -> Maybe a
findImpl a Empty = Nothing
findImpl a (Branch l n r) =
  case compare a n of
    LT -> find a l
    EQ -> Just n
    GT -> find a r

abstractDelete ::
     (Ord a) => BranchCons a (BinaryTree a) -> a -> BinaryTree a -> Maybe (BinaryTree a)
abstractDelete _ _ Empty = Nothing
abstractDelete branch x (Branch l n r) =
  case compare x n of
    LT -> branch <$> abstractDelete branch x l <*> pure n <*> pure r
    EQ ->
      let fromR = (\(rMin, r') -> branch l rMin r') <$> abstractPopHead branch r
          fromL = (\(l', lMax) -> branch l' lMax r) <$> abstractPopLast branch l
       in fromR <|> fromL <|> Just Empty
    GT -> branch l n <$> abstractDelete branch x r

zipTreeWith :: (a -> b -> c) -> BinaryTree a -> BinaryTree b -> BinaryTree c
zipTreeWith _ Empty _ = Empty
zipTreeWith _ _ Empty = Empty
zipTreeWith f (Branch al a ar) (Branch bl b br) =
  Branch (zipTreeWith f al bl) (f a b) (zipTreeWith f ar br)

zipTree :: BinaryTree a -> BinaryTree b -> BinaryTree (a, b)
zipTree = zipTreeWith (,)

unzipTree :: BinaryTree (a, b) -> (BinaryTree a, BinaryTree b)
unzipTree Empty = (Empty, Empty)
unzipTree (Branch l (a, b) r) =
  let (la, lb) = unzipTree l
      (ra, rb) = unzipTree r
   in (Branch la a ra, Branch lb b rb)

isSubgraphOf :: (Eq a) => BinaryTree a -> BinaryTree a -> Bool
isSubgraphOf Empty _ = True
isSubgraphOf (Branch _ _ _) Empty = False
isSubgraphOf a@(Branch al an ar) b@(Branch bl bn br) =
  an == bn && al `isSubgraphOf` bl && ar `isSubgraphOf` br ||
  a `isSubgraphOf` bl || a `isSubgraphOf` br

abstractRotateLeftMaybe :: BranchCons a (BinaryTree a) -> BinaryTree a -> Maybe (BinaryTree a)
abstractRotateLeftMaybe branch (Branch pl pn (Branch cl cn cr)) =
  Just (branch (branch pl pn cl) cn cr)
abstractRotateLeftMaybe _ _ = Nothing

rotateLeftMaybe :: BinaryTree a -> Maybe (BinaryTree a)
rotateLeftMaybe = abstractRotateLeftMaybe Branch

rotateLeft :: BinaryTree a -> BinaryTree a
rotateLeft t = fromMaybe t (rotateLeftMaybe t)

abstractRotateRightMaybe :: BranchCons a (BinaryTree a) -> BinaryTree a -> Maybe (BinaryTree a)
abstractRotateRightMaybe branch (Branch (Branch cl cn cr) pn pr) =
  Just (branch cl cn (branch cr pn pr))
abstractRotateRightMaybe _ _ = Nothing

rotateRightMaybe :: BinaryTree a -> Maybe (BinaryTree a)
rotateRightMaybe = abstractRotateRightMaybe Branch

rotateRight :: BinaryTree a -> BinaryTree a
rotateRight t = fromMaybe t (rotateRightMaybe t)

instance (Ord a, Arbitrary a) => Arbitrary (BinaryTree a) where
  arbitrary = fromList <$> arbitrary

instance SearchTree BinaryTree where
  empty = Empty
  null t =
    case t of
      Empty -> True
      Branch _ _ _ -> False
  insert = abstractInsert Branch
  delete = abstractDelete Branch
  find = findImpl
  head = foldTree leftOrSelf Nothing
    where
      leftOrSelf l n _ = l <|> Just n
  last = foldTree rightOrSelf Nothing
    where
      rightOrSelf _ n r = r <|> Just n
  popHead = abstractPopHead Branch
  popLast = abstractPopLast Branch
