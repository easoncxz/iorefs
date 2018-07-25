module AVLTree where

import BinaryTree (BinaryTree(Branch, Empty), BranchCons, EmptyCons, TreeAlgebra)
import qualified BinaryTree as BT

import Control.Applicative ((<|>))
import Control.Arrow ((***), first, second)
import Data.Coerce
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

branchWithNewHeightAVL :: BranchCons (WithHeight a) (BinaryTree (WithHeight a))
branchWithNewHeightAVL n l r = rebalanceOnce (branchWithNewHeight n l r)

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

popHeadWithHeight :: BinaryTree (WithHeight a) -> Maybe (WithHeight a, BinaryTree (WithHeight a))
popHeadWithHeight = BT.abstractPopHead branchWithNewHeight

popHeadWithHeightAVL :: BinaryTree (WithHeight a) -> Maybe (WithHeight a, BinaryTree (WithHeight a))
popHeadWithHeightAVL = BT.abstractPopHead branchWithNewHeightAVL

popHead :: AVLTree a -> Maybe (a, AVLTree a)
popHead = fmap (whValue *** AVLTree) . popHeadWithHeightAVL . runAVLTree

popLastWithHeight :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a), WithHeight a)
popLastWithHeight = BT.abstractPopLast branchWithNewHeight

popLastWithHeightAVL :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a), WithHeight a)
popLastWithHeightAVL = BT.abstractPopLast branchWithNewHeightAVL

popLast :: AVLTree a -> Maybe (AVLTree a, a)
popLast = fmap (AVLTree *** whValue) . popLastWithHeightAVL . runAVLTree

isAVLBTH :: BinaryTree (WithHeight a) -> Bool
isAVLBTH Empty = True
isAVLBTH b@(Branch _ l r) = isAVLBTH l && isAVLBTH r && ok b
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
