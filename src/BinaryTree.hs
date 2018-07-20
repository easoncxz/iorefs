module BinaryTree where

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Prelude hiding (head, last, tail)

import Test.QuickCheck (Arbitrary(arbitrary), Property, (==>), conjoin, discard)

data BinaryTree a
  = EmptyTree
  | Branch (Node a)
  deriving (Show, Eq, Functor)

data Node a = Node
  { nLabel :: a
  , nLeft :: BinaryTree a
  , nRight :: BinaryTree a
  } deriving (Show, Eq, Functor)

leaf :: a -> BinaryTree a
leaf n = Branch (Node n EmptyTree EmptyTree)

asLeftOf :: BinaryTree a -> a -> BinaryTree a
asLeftOf t a = Branch (Node a t EmptyTree)

asRightOf :: BinaryTree a -> a -> BinaryTree a
asRightOf t a = Branch (Node a EmptyTree t)

asBranch :: BinaryTree a -> Maybe (Node a)
asBranch EmptyTree = Nothing
asBranch (Branch n) = Just n

inorderTraversal :: BinaryTree a -> [a]
inorderTraversal EmptyTree = []
inorderTraversal (Branch (Node n l r)) = inorderTraversal l ++ [n] ++ inorderTraversal r

preorderTraversal :: BinaryTree a -> [a]
preorderTraversal EmptyTree = []
preorderTraversal (Branch (Node n l r)) = [n] ++ preorderTraversal l ++ preorderTraversal r

-- TODO
levelOrderTraversal :: BinaryTree a -> [a]
levelOrderTraversal _ = []

head :: BinaryTree a -> Maybe a
head EmptyTree = Nothing
head (Branch (Node n EmptyTree _)) = Just n
head (Branch (Node _ l _)) = head l

last :: BinaryTree a -> Maybe a
last EmptyTree = Nothing
last (Branch (Node n _ EmptyTree)) = Just n
last (Branch (Node _ _ r)) = last r

rootPrev :: BinaryTree a -> Maybe a
rootPrev EmptyTree = Nothing
rootPrev (Branch (Node _ EmptyTree _)) = Nothing
rootPrev (Branch (Node _ l _)) = last l

rootNext :: BinaryTree a -> Maybe a
rootNext EmptyTree = Nothing
rootNext (Branch (Node _ _ EmptyTree)) = Nothing
rootNext (Branch (Node _ _ r)) = head r

insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert a EmptyTree = leaf a
insert a (Branch (Node n l r)) =
  if a < n
    then Branch (Node n (insert a l) r)
    else Branch (Node n l (insert a r))

fromList :: (Ord a) => [a] -> BinaryTree a
fromList = List.foldl' (flip insert) EmptyTree

instance (Arbitrary a, Ord a) => Arbitrary (BinaryTree a) where
  arbitrary = fromList <$> arbitrary

zipTree :: BinaryTree a -> BinaryTree b -> BinaryTree (a, b)
zipTree EmptyTree _ = EmptyTree
zipTree _ EmptyTree = EmptyTree
zipTree (Branch (Node a al ar)) (Branch (Node b bl br)) =
  Branch (Node (a, b) (zipTree al bl) (zipTree ar br))

unzipTree :: BinaryTree (a, b) -> (BinaryTree a, BinaryTree b)
unzipTree EmptyTree = (EmptyTree, EmptyTree)
unzipTree (Branch (Node (a, b) l r)) =
  let (la, lb) = unzipTree l
      (ra, rb) = unzipTree r
   in (Branch (Node a la ra), Branch (Node b lb rb))

emptyTreeHeight :: Int
emptyTreeHeight = -1

wouldBeHeight :: Maybe Int -> Maybe Int -> Int
wouldBeHeight l r = 1 + fromMaybe emptyTreeHeight (max l r)

heightTree :: BinaryTree a -> BinaryTree Int
heightTree EmptyTree = EmptyTree
heightTree (Branch (Node _ l r)) =
  let lt = heightTree l
      rt = heightTree r
      h = wouldBeHeight (nLabel <$> asBranch lt) (nLabel <$> asBranch rt)
   in Branch (Node h lt rt)

withHeight :: BinaryTree t -> BinaryTree (t, Int)
withHeight t = zipTree t (heightTree t)

readHeightMay :: BinaryTree (a, Int) -> Maybe Int
readHeightMay EmptyTree = Nothing
readHeightMay (Branch (Node (_, h) _ _)) = Just h

readHeight :: BinaryTree (a, Int) -> Int
readHeight = fromMaybe emptyTreeHeight . readHeightMay

nodeWithHeight :: t -> BinaryTree (t, Int) -> BinaryTree (t, Int) -> Node (t, Int)
nodeWithHeight n l r = Node (n, wouldBeHeight (readHeightMay l) (readHeightMay r)) l r

leafWithHeight :: t -> BinaryTree (t, Int)
leafWithHeight n = Branch (nodeWithHeight n EmptyTree EmptyTree)

insertWithHeight :: (Ord a) => a -> BinaryTree (a, Int) -> BinaryTree (a, Int)
insertWithHeight a EmptyTree = leafWithHeight a
insertWithHeight a (Branch (Node (n, h) l r)) =
  if a < n
    then Branch (nodeWithHeight n (insertWithHeight a l) r)
    else Branch (nodeWithHeight n l (insertWithHeight a r))

isSubtreeOf :: (Eq a) => BinaryTree a -> BinaryTree a -> Bool
isSubtreeOf EmptyTree _ = True
isSubtreeOf (Branch _) EmptyTree = False
isSubtreeOf a@(Branch (Node an al ar)) b@(Branch (Node bn bl br)) =
  an == bn && al `isSubtreeOf` bl && ar `isSubtreeOf` br || a `isSubtreeOf` bl || a `isSubtreeOf` br

newtype BalanceFactor = BalanceFactor
  { runBalanceFactor :: Int
  } deriving (Show, Eq, Num, Ord)

data Balance a
  = LeftTaller BalanceFactor
               (Node (a, Int))
  | SameHeight
  | RightTaller BalanceFactor
                (Node (a, Int))
  deriving (Show, Eq)

balance :: BinaryTree (a, Int) -> Balance a
balance EmptyTree = SameHeight
balance (Branch (Node _ EmptyTree EmptyTree)) = SameHeight
balance (Branch (Node _ l@(Branch n) EmptyTree)) =
  LeftTaller (BalanceFactor (readHeight l - readHeight EmptyTree)) n
balance (Branch (Node _ EmptyTree r@(Branch n))) =
  RightTaller (BalanceFactor (readHeight EmptyTree - readHeight r)) n
balance (Branch (Node _ l@(Branch ln) r@(Branch rn))) =
  let factor = readHeight l - readHeight r
   in case compare factor 0 of
        GT -> LeftTaller (BalanceFactor factor) ln
        EQ -> SameHeight
        LT -> RightTaller (BalanceFactor factor) rn

readBalanceFactor :: Balance a -> Int
readBalanceFactor (LeftTaller (BalanceFactor f) _) = f
readBalanceFactor SameHeight = 0
readBalanceFactor (RightTaller (BalanceFactor f) _) = f

balanceFactor :: BinaryTree (a, Int) -> Int
balanceFactor = readBalanceFactor . balance

nodeAdmissable :: BinaryTree (a, Int) -> Bool
nodeAdmissable t = balanceFactor t `elem` [-1 .. 1]

isAVL :: (Ord a) => BinaryTree a -> Bool
isAVL t = go (withHeight t)
  where
    go EmptyTree = True
    go b@(Branch (Node _ l r)) = nodeAdmissable b && go l && go r

rotateLeft :: BinaryTree (a, Int) -> BinaryTree (a, Int)
rotateLeft EmptyTree = EmptyTree
rotateLeft b@(Branch (Node _ _ EmptyTree)) = b
rotateLeft (Branch (Node (pn, _) pl (Branch (Node (cn, _) cl cr)))) =
  Branch (nodeWithHeight cn (Branch (nodeWithHeight pn pl cl)) cr)

rotateLeftMaybe :: BinaryTree (a, Int) -> Maybe (BinaryTree (a, Int))
rotateLeftMaybe EmptyTree = Nothing
rotateLeftMaybe (Branch (Node _ _ EmptyTree)) = Nothing
rotateLeftMaybe b = Just (rotateLeft b)

rotateRight :: BinaryTree (a, Int) -> BinaryTree (a, Int)
rotateRight EmptyTree = EmptyTree
rotateRight b@(Branch (Node _ EmptyTree EmptyTree)) = b
rotateRight b@(Branch (Node _ EmptyTree Branch {})) = b
rotateRight p@(Branch (Node (pn, _) c@(Branch (Node (cn, _) cl cr)) pr)) =
  Branch (nodeWithHeight cn cl (Branch (nodeWithHeight pn cr pr)))

rotateRightMaybe :: BinaryTree (a, Int) -> Maybe (BinaryTree (a, Int))
rotateRightMaybe EmptyTree = Nothing
rotateRightMaybe (Branch (Node _ EmptyTree _)) = Nothing
rotateRightMaybe b = Just (rotateRight b)

rebalanceOnce :: BinaryTree (a, Int) -> BinaryTree (a, Int)
rebalanceOnce EmptyTree = EmptyTree
rebalanceOnce p@(Branch (Node (pn, ph) pl pr)) =
  case balance p of
    LeftTaller f ln
      | f > 1 ->
        case balance (Branch ln) of
          LeftTaller _ _ -> rotateRight p
          SameHeight -> rotateRight p
          RightTaller _ _ -> rotateRight (Branch (nodeWithHeight pn (rotateLeft (Branch ln)) pr))
    RightTaller f rn
      | f < -1 ->
        case balance (Branch rn) of
          RightTaller _ _ -> rotateLeft p
          SameHeight -> rotateLeft p
          LeftTaller _ _ -> rotateLeft (Branch (nodeWithHeight pn pl (rotateRight (Branch rn))))
    _ -> p

insertWithHeightAVL :: (Ord a) => a -> BinaryTree (a, Int) -> BinaryTree (a, Int)
insertWithHeightAVL a EmptyTree = leafWithHeight a
insertWithHeightAVL a (Branch (Node (n, h) l r)) =
  if a < n
    then rebalanceOnce (Branch (nodeWithHeight n (insertWithHeightAVL a l) r))
    else rebalanceOnce (Branch (nodeWithHeight n l (insertWithHeightAVL a r)))

fromListAVL :: (Ord a) => [a] -> BinaryTree (a, Int)
fromListAVL = List.foldl' (flip insertWithHeightAVL) EmptyTree
