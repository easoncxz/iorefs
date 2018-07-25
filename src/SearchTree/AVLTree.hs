module SearchTree.AVLTree where

import SearchTree.BinaryTree (BinaryTree(Branch, Empty), BranchCons, EmptyCons, TreeAlgebra)
import qualified SearchTree.BinaryTree as BT

import Control.Applicative ((<|>))
import Control.Arrow ((***), first, second)
import Data.Coerce
import Data.Function (on)
import qualified Data.List as List
import Data.Maybe (fromMaybe)

import Test.QuickCheck (Arbitrary, arbitrary)

data WithHeight a = WithHeight
  { whHeight :: Int
  , whValue :: a
  } deriving (Show, Eq)

instance Ord a => Ord (WithHeight a) where
  compare = compare `on` whValue

instance Functor WithHeight where
  fmap f (WithHeight h a) = WithHeight h (f a)

instance Foldable WithHeight where
  foldr f z (WithHeight _ a) = f a z

newtype AVLTree a = AVLTree
  { runAVLTree :: BinaryTree (WithHeight a)
  } deriving (Show, Eq, Functor, Foldable)

empty :: AVLTree a
empty = AVLTree Empty

null :: AVLTree a -> Bool
null = BT.null . runAVLTree

emptyTreeHeight :: Int
emptyTreeHeight = -1

wouldBeHeight :: Int -> Int -> Int
wouldBeHeight l r = 1 + (max l r)

heightTree :: BinaryTree a -> BinaryTree Int
heightTree = snd . BT.foldTree build (emptyTreeHeight, Empty)
  where
    build (lh, lt) n (rh, rt) =
      let h = wouldBeHeight lh rh
       in (h, Branch lt h rt)

readHeight :: BinaryTree (WithHeight a) -> Int
readHeight = fromMaybe emptyTreeHeight . fmap whHeight . BT.node

branchWithHeight ::
     BinaryTree (WithHeight t) -> t -> BinaryTree (WithHeight t) -> BinaryTree (WithHeight t)
branchWithHeight l n r =
  let h = wouldBeHeight (readHeight l) (readHeight r)
   in Branch l (WithHeight h n) r

branchWithNewHeight :: BranchCons (WithHeight a) (BinaryTree (WithHeight a))
branchWithNewHeight l n r = branchWithHeight l (whValue n) r

branchWithNewHeightAVL :: BranchCons (WithHeight a) (BinaryTree (WithHeight a))
branchWithNewHeightAVL l n r = rebalanceOnce (branchWithNewHeight l n r)

withHeightAlgebra :: TreeAlgebra (WithHeight a) (BinaryTree (WithHeight a))
withHeightAlgebra = (Empty, branchWithNewHeight)

withHeightAVLAlgebra :: TreeAlgebra (WithHeight a) (BinaryTree (WithHeight a))
withHeightAVLAlgebra = (Empty, branchWithNewHeightAVL)

treeWithHeight :: BinaryTree a -> BinaryTree (WithHeight a)
treeWithHeight = BT.foldTree branchWithHeight Empty

treeWithoutHeight :: BinaryTree (WithHeight a) -> BinaryTree a
treeWithoutHeight = fmap whValue

treeWithNewHeight :: BinaryTree (WithHeight t) -> BinaryTree (WithHeight t)
treeWithNewHeight = BT.foldTree branchWithNewHeight Empty

head :: AVLTree a -> Maybe a
head = fmap whValue . BT.head . runAVLTree

popHeadWithHeight :: BinaryTree (WithHeight a) -> Maybe (WithHeight a, BinaryTree (WithHeight a))
popHeadWithHeight = BT.abstractPopHead branchWithNewHeight

popHeadWithHeightAVL :: BinaryTree (WithHeight a) -> Maybe (WithHeight a, BinaryTree (WithHeight a))
popHeadWithHeightAVL = BT.abstractPopHead branchWithNewHeightAVL

popHead :: AVLTree a -> Maybe (a, AVLTree a)
popHead = fmap (whValue *** AVLTree) . popHeadWithHeightAVL . runAVLTree

last :: AVLTree a -> Maybe a
last = fmap whValue . BT.last . runAVLTree

popLastWithHeight :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a), WithHeight a)
popLastWithHeight = BT.abstractPopLast branchWithNewHeight

popLastWithHeightAVL :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a), WithHeight a)
popLastWithHeightAVL = BT.abstractPopLast branchWithNewHeightAVL

popLast :: AVLTree a -> Maybe (AVLTree a, a)
popLast = fmap (AVLTree *** whValue) . popLastWithHeightAVL . runAVLTree

isAVLBTH :: BinaryTree (WithHeight a) -> Bool
isAVLBTH Empty = True
isAVLBTH b@(Branch l _ r) = isAVLBTH l && isAVLBTH r && ok b
  where
    ok t = fst (analyseBalance t) == Balanced

validAVL :: AVLTree a -> Bool
validAVL (AVLTree t) = isAVLBTH t

rotateLeftMaybe :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
rotateLeftMaybe = BT.abstractRotateLeftMaybe branchWithNewHeight

rotateLeft :: BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
rotateLeft t = fromMaybe t (rotateLeftMaybe t)

rotateRightMaybe :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
rotateRightMaybe = BT.abstractRotateRightMaybe branchWithNewHeight

rotateRight :: BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
rotateRight t = fromMaybe t (rotateRightMaybe t)

data IsBalanced
  = NotBalanced
  | Balanced
  deriving (Show, Eq)

data TallerSide
  = LeftTaller
  | NeitherTaller
  | RightTaller
  deriving (Show, Eq)

analyseBalance :: BinaryTree (WithHeight a) -> (IsBalanced, TallerSide)
analyseBalance Empty = (Balanced, NeitherTaller)
analyseBalance (Branch l _ r) =
  let lh = readHeight l
      rh = readHeight r
      isBalanced =
        if abs (lh - rh) <= 1
          then Balanced
          else NotBalanced
      tallerSide =
        case compare lh rh of
          LT -> RightTaller
          EQ -> NeitherTaller
          GT -> LeftTaller
   in (isBalanced, tallerSide)

rebalanceOnce :: BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
rebalanceOnce Empty = Empty
rebalanceOnce (Branch Empty n Empty) = Branch Empty n Empty
rebalanceOnce b@(Branch l n r) =
  let errTooMuch = error "Imbalance in tree got out of control"
      errNonsenseAnalysis =
        error
          "Nonsense output from analyseBalance: neither side is taller but tree is not balanced?!"
   in case analyseBalance b of
        (Balanced, _) -> b
        (_, NeitherTaller) -> errNonsenseAnalysis
        (_, LeftTaller) ->
          case analyseBalance l of
            (NotBalanced, _) -> errTooMuch
            (_, RightTaller) -> rotateRight (branchWithNewHeight (rotateLeft l) n r)
            (_, _) -> rotateRight b
        (_, RightTaller) ->
          case analyseBalance r of
            (NotBalanced, _) -> errTooMuch
            (_, LeftTaller) -> rotateLeft (branchWithNewHeight l n (rotateRight r))
            (_, _) -> rotateLeft b

insertWithHeight ::
     (Ord a) => WithHeight a -> BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
insertWithHeight = BT.abstractInsert withHeightAlgebra

insertWithHeightAVL ::
     (Ord a) => WithHeight a -> BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
insertWithHeightAVL = BT.abstractInsert withHeightAVLAlgebra

insert :: (Ord a) => a -> AVLTree a -> AVLTree a
insert x (AVLTree t) = AVLTree (insertWithHeightAVL (WithHeight undefined x) t)

deleteWithHeight ::
     (Ord a) => WithHeight a -> BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
deleteWithHeight = BT.abstractDelete withHeightAlgebra

deleteWithHeightAVL ::
     (Ord a) => WithHeight a -> BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
deleteWithHeightAVL = BT.abstractDelete withHeightAVLAlgebra

delete :: (Ord a) => a -> AVLTree a -> Maybe (AVLTree a)
delete x (AVLTree t) = AVLTree <$> deleteWithHeightAVL (WithHeight undefined x) t

fromList :: (Ord a) => [a] -> AVLTree a
fromList = List.foldl' (flip insert) (AVLTree Empty)

instance (Arbitrary a, Ord a) => Arbitrary (AVLTree a) where
  arbitrary = fromList <$> arbitrary
