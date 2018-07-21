module AVLTree where

import BinaryTree (BinaryTree(Branch, Empty))
import qualified BinaryTree as BT

import qualified Data.List as List
import Data.Maybe (fromMaybe)

type Height = Int

newtype AVLTree a = AVLTree
  { runAVLTree :: BinaryTree (a, Height)
  } deriving (Show, Eq)

emptyTreeHeight :: Height
emptyTreeHeight = -1

wouldBeHeight :: Height -> Height -> Height
wouldBeHeight l r = 1 + (max l r)

heightTree :: BinaryTree a -> BinaryTree Height
heightTree Empty = Empty
heightTree (Branch _ l r) =
  let lt = heightTree l
      rt = heightTree r
      h =
        wouldBeHeight
          (fromMaybe emptyTreeHeight (BT.node lt))
          (fromMaybe emptyTreeHeight (BT.node rt))
   in Branch h lt rt

withHeight :: BinaryTree a -> BinaryTree (a, Height)
withHeight t = BT.zipTree t (heightTree t)

fromBinaryTree :: BinaryTree a -> AVLTree a
fromBinaryTree t = AVLTree (BT.zipTree t (heightTree t))

readHeight :: BinaryTree (a, Int) -> Int
readHeight = fromMaybe emptyTreeHeight . fmap snd . BT.node

branchWithHeight ::
     t -> BinaryTree (t, Int) -> BinaryTree (t, Int) -> BinaryTree (t, Int)
branchWithHeight n l r =
  Branch (n, wouldBeHeight (readHeight l) (readHeight r)) l r

leafWithHeight :: t -> BinaryTree (t, Int)
leafWithHeight n = branchWithHeight n Empty Empty

insertWithHeight :: (Ord a) => a -> BinaryTree (a, Int) -> BinaryTree (a, Int)
insertWithHeight a Empty = leafWithHeight a
insertWithHeight a (Branch (n, h) l r) =
  if a < n
    then branchWithHeight n (insertWithHeight a l) r
    else branchWithHeight n l (insertWithHeight a r)

newtype BalanceFactor = BalanceFactor
  { runBalanceFactor :: Int
  } deriving (Show, Eq, Num, Ord)

data Balance a
  = LeftTaller BalanceFactor
               (a, Int)
               (BinaryTree (a, Int))
               (BinaryTree (a, Int))
  | SameHeight
  | RightTaller BalanceFactor
                (a, Int)
                (BinaryTree (a, Int))
                (BinaryTree (a, Int))
  deriving (Show, Eq)

balance :: BinaryTree (a, Int) -> Balance a
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

balanceFactor :: BinaryTree (a, Int) -> Int
balanceFactor = readBalanceFactor . balance

isAVL :: (Ord a) => BinaryTree a -> Bool
isAVL t = go (withHeight t)
  where
    go :: BinaryTree (a, Int) -> Bool
    go Empty = True
    go b@(Branch _ l r) = nodeAdmissable b && go l && go r
    nodeAdmissable :: BinaryTree (a, Int) -> Bool
    nodeAdmissable t = balanceFactor t `elem` [-1 .. 1]

rotateLeft :: BinaryTree (a, Int) -> BinaryTree (a, Int)
rotateLeft Empty = Empty
rotateLeft b@(Branch _ _ Empty) = b
rotateLeft (Branch (pn, _) pl (Branch (cn, _) cl cr)) =
  branchWithHeight cn (branchWithHeight pn pl cl) cr

rotateLeftMaybe :: BinaryTree (a, Int) -> Maybe (BinaryTree (a, Int))
rotateLeftMaybe Empty = Nothing
rotateLeftMaybe (Branch _ _ Empty) = Nothing
rotateLeftMaybe b = Just (rotateLeft b)

rotateRight :: BinaryTree (a, Int) -> BinaryTree (a, Int)
rotateRight Empty = Empty
rotateRight b@(Branch _ Empty Empty) = b
rotateRight b@(Branch _ Empty Branch {}) = b
rotateRight p@(Branch (pn, _) c@(Branch (cn, _) cl cr) pr) =
  branchWithHeight cn cl (branchWithHeight pn cr pr)

rotateRightMaybe :: BinaryTree (a, Int) -> Maybe (BinaryTree (a, Int))
rotateRightMaybe Empty = Nothing
rotateRightMaybe (Branch _ Empty _) = Nothing
rotateRightMaybe b = Just (rotateRight b)

rebalanceOnce :: BinaryTree (a, Int) -> BinaryTree (a, Int)
rebalanceOnce Empty = Empty
rebalanceOnce p@(Branch (pn, ph) pl pr) =
  case balance p of
    LeftTaller f ln ll lr
      | f > 1 ->
        let lt = Branch ln ll lr
         in case balance lt of
              LeftTaller _ _ _ _ -> rotateRight p
              SameHeight -> rotateRight p
              RightTaller _ _ _ _ ->
                rotateRight (branchWithHeight pn (rotateLeft lt) pr)
    RightTaller f rn rl rr
      | f < -1 ->
        let rt = Branch rn rl rr
         in case balance rt of
              RightTaller _ _ _ _ -> rotateLeft p
              SameHeight -> rotateLeft p
              LeftTaller _ _ _ _ ->
                rotateLeft (branchWithHeight pn pl (rotateRight rt))
    _ -> p

insertWithHeightAVL ::
     (Ord a) => a -> BinaryTree (a, Int) -> BinaryTree (a, Int)
insertWithHeightAVL a Empty = leafWithHeight a
insertWithHeightAVL a (Branch (n, h) l r) =
  if a < n
    then rebalanceOnce (branchWithHeight n (insertWithHeightAVL a l) r)
    else rebalanceOnce (branchWithHeight n l (insertWithHeightAVL a r))

fromListAVL :: (Ord a) => [a] -> BinaryTree (a, Int)
fromListAVL = List.foldl' (flip insertWithHeightAVL) Empty
