module AVLTree where

import BinaryTree (BinaryTree(Branch, Empty))
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

branchWithNewHeight ::
     WithHeight t
  -> BinaryTree (WithHeight t)
  -> BinaryTree (WithHeight t)
  -> BinaryTree (WithHeight t)
branchWithNewHeight = branchWithHeight . whValue

leafWithHeight :: t -> BinaryTree (WithHeight t)
leafWithHeight n = branchWithHeight n Empty Empty

treeWithHeight :: BinaryTree a -> BinaryTree (WithHeight a)
treeWithHeight = BT.foldTree branchWithHeight Empty

treeWithoutHeight :: BinaryTree (WithHeight a) -> BinaryTree a
treeWithoutHeight = fmap whValue

treeWithNewHeight :: BinaryTree (WithHeight t) -> BinaryTree (WithHeight t)
treeWithNewHeight = BT.foldTree branchWithNewHeight Empty

fromBinaryTree :: BinaryTree a -> AVLTree a
fromBinaryTree = AVLTree . treeWithHeight

empty :: AVLTree a
empty = AVLTree Empty

branch :: a -> AVLTree a -> AVLTree a -> AVLTree a
branch n (AVLTree l) (AVLTree r) =
  let h = wouldBeHeight (readHeight l) (readHeight r)
   in AVLTree (Branch (WithHeight h n) l r)

leaf :: a -> AVLTree a
leaf n = branch n empty empty

insertWithHeight :: (Ord a) => a -> BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
insertWithHeight a Empty = leafWithHeight a
insertWithHeight a (Branch (WithHeight h n) l r) =
  if a < n
    then branchWithHeight n (insertWithHeight a l) r
    else branchWithHeight n l (insertWithHeight a r)

popHeadWithHeight :: BinaryTree (WithHeight a) -> Maybe (WithHeight a, BinaryTree (WithHeight a))
popHeadWithHeight Empty = Nothing
popHeadWithHeight (Branch (WithHeight h n) l r) =
  (fmap . second) (\l' -> branchWithHeight n l' r) (popHeadWithHeight l) <|>
  Just (WithHeight h n, r)

popHeadWithHeightAVL :: BinaryTree (WithHeight a) -> Maybe (WithHeight a, BinaryTree (WithHeight a))
popHeadWithHeightAVL Empty = Nothing
popHeadWithHeightAVL (Branch (WithHeight h n) l r) =
  (fmap . second) (\l' -> rebalanceOnce (branchWithHeight n l' r)) (popHeadWithHeightAVL l) <|>
  Just (WithHeight h n, r)

popLastWithHeight :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a), WithHeight a)
popLastWithHeight Empty = Nothing
popLastWithHeight (Branch (WithHeight h n) l r) =
  (fmap . first) (\r' -> branchWithHeight n l r') (popLastWithHeight r) <|> Just (l, WithHeight h n)

popLastWithHeightAVL :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a), WithHeight a)
popLastWithHeightAVL Empty = Nothing
popLastWithHeightAVL (Branch (WithHeight h n) l r) =
  (fmap . first) (\r' -> rebalanceOnce (branchWithHeight n l r')) (popLastWithHeightAVL r) <|>
  Just (l, WithHeight h n)

deleteWithHeight :: (Ord a) => a -> BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
deleteWithHeight _ Empty = Nothing
deleteWithHeight x (Branch (WithHeight _ n) l r) =
  case compare x n of
    LT -> branchWithHeight <$> pure n <*> deleteWithHeight x l <*> pure r
    EQ ->
      let fromR = (\(WithHeight _ rMin, r') -> branchWithHeight rMin l r') <$> popHeadWithHeight r
          fromL = (\(l', WithHeight _ lMax) -> branchWithHeight lMax l' r) <$> popLastWithHeight l
       in fromR <|> fromL <|> Just Empty
    GT -> branchWithHeight n l <$> deleteWithHeight x r

newtype BalanceFactor = BalanceFactor
  { runBalanceFactor :: Int
  } deriving (Show, Eq, Num, Ord)

data Balance a
  = LeftTaller BalanceFactor
               (WithHeight a)
               (BinaryTree (WithHeight a))
               (BinaryTree (WithHeight a))
  | SameHeight
  | RightTaller BalanceFactor
                (WithHeight a)
                (BinaryTree (WithHeight a))
                (BinaryTree (WithHeight a))
  deriving (Show, Eq)

balance :: BinaryTree (WithHeight a) -> Balance a
balance Empty = SameHeight
balance (Branch _ Empty Empty) = SameHeight
balance (Branch _ lt@(Branch n l r) Empty) =
  LeftTaller (BalanceFactor (readHeight lt - readHeight Empty)) n l r
balance (Branch _ Empty rt@(Branch n l r)) =
  RightTaller (BalanceFactor (readHeight Empty - readHeight rt)) n l r
balance (Branch _ lt@(Branch ln ll lr) rt@(Branch rn rl rr)) =
  let factor = readHeight lt - readHeight rt
   in case compare factor 0 of
        GT -> LeftTaller (BalanceFactor factor) ln ll lr
        EQ -> SameHeight
        LT -> RightTaller (BalanceFactor factor) rn rl rr

readBalanceFactor :: Balance a -> Int
readBalanceFactor (LeftTaller (BalanceFactor f) _ _ _) = f
readBalanceFactor SameHeight = 0
readBalanceFactor (RightTaller (BalanceFactor f) _ _ _) = f

balanceFactor :: BinaryTree (WithHeight a) -> Int
balanceFactor = readBalanceFactor . balance

isAVL :: (Ord a) => BinaryTree a -> Bool
isAVL t = go (treeWithHeight t)
  where
    go :: BinaryTree (WithHeight a) -> Bool
    go Empty = True
    go b@(Branch _ l r) = nodeAdmissable b && go l && go r
    nodeAdmissable :: BinaryTree (WithHeight a) -> Bool
    nodeAdmissable t = balanceFactor t `elem` [-1 .. 1]

rotateLeft :: BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
rotateLeft Empty = Empty
rotateLeft b@(Branch _ _ Empty) = b
rotateLeft (Branch (WithHeight _ pn) pl (Branch (WithHeight _ cn) cl cr)) =
  branchWithHeight cn (branchWithHeight pn pl cl) cr

rotateLeftMaybe :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
rotateLeftMaybe Empty = Nothing
rotateLeftMaybe (Branch _ _ Empty) = Nothing
rotateLeftMaybe b = Just (rotateLeft b)

rotateRight :: BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
rotateRight Empty = Empty
rotateRight b@(Branch _ Empty Empty) = b
rotateRight b@(Branch _ Empty Branch {}) = b
rotateRight p@(Branch (WithHeight _ pn) c@(Branch (WithHeight _ cn) cl cr) pr) =
  branchWithHeight cn cl (branchWithHeight pn cr pr)

rotateRightMaybe :: BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
rotateRightMaybe Empty = Nothing
rotateRightMaybe (Branch _ Empty _) = Nothing
rotateRightMaybe b = Just (rotateRight b)

rebalanceOnce :: BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
rebalanceOnce Empty = Empty
rebalanceOnce p@(Branch (WithHeight ph pn) pl pr) =
  case balance p of
    LeftTaller f ln ll lr
      | f > 1 ->
        let lt = Branch ln ll lr
         in case balance lt of
              LeftTaller _ _ _ _ -> rotateRight p
              SameHeight -> rotateRight p
              RightTaller _ _ _ _ -> rotateRight (branchWithHeight pn (rotateLeft lt) pr)
    RightTaller f rn rl rr
      | f < -1 ->
        let rt = Branch rn rl rr
         in case balance rt of
              RightTaller _ _ _ _ -> rotateLeft p
              SameHeight -> rotateLeft p
              LeftTaller _ _ _ _ -> rotateLeft (branchWithHeight pn pl (rotateRight rt))
    _ -> p

insertWithHeightAVL :: (Ord a) => a -> BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)
insertWithHeightAVL a Empty = leafWithHeight a
insertWithHeightAVL a (Branch (WithHeight h n) l r) =
  rebalanceOnce $
  if a < n
    then branchWithHeight n (insertWithHeightAVL a l) r
    else branchWithHeight n l (insertWithHeightAVL a r)

deleteWithHeightAVL ::
     (Ord a) => a -> BinaryTree (WithHeight a) -> Maybe (BinaryTree (WithHeight a))
deleteWithHeightAVL _ Empty = Nothing
deleteWithHeightAVL x (Branch (WithHeight _ n) l r) =
  rebalanceOnce <$>
  case compare x n of
    LT -> branchWithHeight <$> pure n <*> deleteWithHeightAVL x l <*> pure r
    EQ ->
      let deleteR =
            (\(WithHeight _ rMin, r') -> branchWithHeight rMin l r') <$> popHeadWithHeightAVL r
          deleteL =
            (\(l', WithHeight _ lMax) -> branchWithHeight lMax l' r) <$> popLastWithHeightAVL l
       in deleteR <|> deleteL <|> Just Empty
    GT -> branchWithHeight n l <$> deleteWithHeightAVL x r

liftBT :: (BinaryTree (WithHeight a) -> BinaryTree (WithHeight a)) -> AVLTree a -> AVLTree a
liftBT f = AVLTree . f . runAVLTree

insert :: (Ord a) => a -> AVLTree a -> AVLTree a
insert = liftBT . insertWithHeightAVL

fromList :: (Ord a) => [a] -> AVLTree a
fromList = AVLTree . List.foldl' (flip insertWithHeightAVL) Empty

instance (Arbitrary a, Ord a) => Arbitrary (AVLTree a) where
  arbitrary = fromList <$> arbitrary
