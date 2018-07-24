module AVLTree where

import BinaryTree (BinaryTree(Branch, Empty), BranchCons, EmptyCons, TreeAlgebra)
import qualified BinaryTree as BT

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Data.Function (on)
import qualified Data.List as List
import Data.Maybe (fromMaybe)

import Test.QuickCheck (Arbitrary, arbitrary)

type Height = Int

data WithHeight a = WithHeight
  { whHeight :: Int
  , whValue :: a
  } deriving (Show, Eq)

instance Ord a => Ord (WithHeight a) where
  compare = compare `on` whValue

newtype AVLTree a = AVLTree
  { runAVLTree :: BinaryTree (WithHeight a)
  } deriving (Show, Eq)

emptyTreeHeight :: Height
emptyTreeHeight = -1

wouldBeHeight :: Height -> Height -> Height
wouldBeHeight l r = 1 + (max l r)

heightTree :: BinaryTree a -> BinaryTree Int
heightTree = snd . BT.foldTree build (emptyTreeHeight, Empty)
  where
    build n (lh, lt) (rh, rt) =
      let h = wouldBeHeight lh rh
       in (h, Branch h lt rt)

readHeight :: BinaryTree (WithHeight a) -> Int
readHeight = fromMaybe emptyTreeHeight . fmap whHeight . BT.node

branchWithHeight ::
     t -> BinaryTree (WithHeight t) -> BinaryTree (WithHeight t) -> BinaryTree (WithHeight t)
branchWithHeight n l r =
  let h = wouldBeHeight (readHeight l) (readHeight r)
   in Branch (WithHeight h n) l r

branchWithNewHeight :: BranchCons (WithHeight a) (BinaryTree (WithHeight a))
branchWithNewHeight = branchWithHeight . whValue

branchWithNewHeightAndRebalancing :: BranchCons (WithHeight a) (BinaryTree (WithHeight a))
branchWithNewHeightAndRebalancing n l r = rebalanceOnce (branchWithNewHeight n l r)

withHeightAlgebra :: TreeAlgebra (WithHeight a) (BinaryTree (WithHeight a))
withHeightAlgebra = (Empty, branchWithNewHeight)

withHeightAndRebalancingAlgebra :: TreeAlgebra (WithHeight a) (BinaryTree (WithHeight a))
withHeightAndRebalancingAlgebra = (Empty, branchWithNewHeightAndRebalancing)

treeWithHeight :: BinaryTree a -> BinaryTree (WithHeight a)
treeWithHeight = BT.foldTree branchWithHeight Empty

treeWithoutHeight :: BinaryTree (WithHeight a) -> BinaryTree a
treeWithoutHeight = fmap whValue

treeWithNewHeight :: BinaryTree (WithHeight t) -> BinaryTree (WithHeight t)
treeWithNewHeight = BT.foldTree branchWithNewHeight Empty

empty :: AVLTree a
empty = AVLTree Empty

branch :: a -> AVLTree a -> AVLTree a -> AVLTree a
branch n (AVLTree l) (AVLTree r) =
  let h = wouldBeHeight (readHeight l) (readHeight r)
   in AVLTree (Branch (WithHeight h n) l r)

leaf :: a -> AVLTree a
leaf n = branch n empty empty

insertWithHeight ::
     (Ord a) => WithHeight a -> BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
insertWithHeight = BT.abstractInsert withHeightAlgebra

popHeadWithHeight :: BinaryTree (WithHeight a) -> Maybe (WithHeight a, BinaryTree (WithHeight a))
popHeadWithHeight = BT.abstractPopHead branchWithNewHeight

popHeadWithHeightAVL :: BinaryTree (WithHeight a) -> Maybe (WithHeight a, BinaryTree (WithHeight a))
popHeadWithHeightAVL = BT.abstractPopHead branchWithNewHeightAndRebalancing

popLastWithHeight :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a), WithHeight a)
popLastWithHeight = BT.abstractPopLast branchWithNewHeight

popLastWithHeightAVL :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a), WithHeight a)
popLastWithHeightAVL = BT.abstractPopLast branchWithNewHeightAndRebalancing

deleteWithHeight ::
     (Ord a) => WithHeight a -> BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
deleteWithHeight = BT.abstractDelete withHeightAlgebra

isAVL :: (Ord a) => BinaryTree a -> Bool
isAVL t = go (treeWithHeight t)
  where
    go :: BinaryTree (WithHeight a) -> Bool
    go Empty = True
    go b@(Branch _ l r) = nodeAdmissable b && go l && go r
    nodeAdmissable :: BinaryTree (WithHeight a) -> Bool
    nodeAdmissable t = fst (analyseBalance t) == Balanced

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
analyseBalance (Branch _ l r) =
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
rebalanceOnce (Branch n Empty Empty) = Branch n Empty Empty
rebalanceOnce b@(Branch n l r) =
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
            (_, RightTaller) -> rotateRight (branchWithNewHeight n (rotateLeft l) r)
            (_, _) -> rotateRight b
        (_, RightTaller) ->
          case analyseBalance r of
            (NotBalanced, _) -> errTooMuch
            (_, LeftTaller) -> rotateLeft (branchWithNewHeight n l (rotateRight r))
            (_, _) -> rotateLeft b

insertWithHeightAVL ::
     (Ord a) => WithHeight a -> BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
insertWithHeightAVL = BT.abstractInsert withHeightAndRebalancingAlgebra

deleteWithHeightAVL ::
     (Ord a) => WithHeight a -> BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
deleteWithHeightAVL = BT.abstractDelete withHeightAndRebalancingAlgebra

liftBT :: (BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)) -> AVLTree a -> AVLTree a
liftBT f = AVLTree . f . runAVLTree

insert :: (Ord a) => a -> AVLTree a -> AVLTree a
insert = liftBT . insertWithHeightAVL . WithHeight undefined

fromList :: (Ord a) => [a] -> AVLTree a
fromList = AVLTree . List.foldl' (flip insertWithHeightAVL) Empty . fmap (WithHeight undefined)

instance (Arbitrary a, Ord a) => Arbitrary (AVLTree a) where
  arbitrary = fromList <$> arbitrary
