module BinaryTree where

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Prelude hiding (head, last, tail)

import Test.QuickCheck (Arbitrary(arbitrary), Property, (==>), conjoin, discard)

data BinaryTree a
  = EmptyTree
  | Branch a
           (BinaryTree a)
           (BinaryTree a)
  deriving (Show, Eq, Functor)

null :: BinaryTree a -> Bool
null EmptyTree = True
null (Branch _ _ _) = False

leaf :: a -> BinaryTree a
leaf n = Branch n EmptyTree EmptyTree

asLeftOf :: BinaryTree a -> a -> BinaryTree a
asLeftOf t a = Branch a t EmptyTree

asRightOf :: BinaryTree a -> a -> BinaryTree a
asRightOf t a = Branch a EmptyTree t

node :: BinaryTree a -> Maybe a
node EmptyTree = Nothing
node (Branch n _ _) = Just n

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

insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert a EmptyTree = leaf a
insert a (Branch n l r) =
  if a < n
    then Branch n (insert a l) r
    else Branch n l (insert a r)

fromList :: (Ord a) => [a] -> BinaryTree a
fromList = List.foldl' (flip insert) EmptyTree

instance (Arbitrary a, Ord a) => Arbitrary (BinaryTree a) where
  arbitrary = fromList <$> arbitrary

zipTree :: BinaryTree a -> BinaryTree b -> BinaryTree (a, b)
zipTree EmptyTree _ = EmptyTree
zipTree _ EmptyTree = EmptyTree
zipTree (Branch a al ar) (Branch b bl br) =
  Branch (a, b) (zipTree al bl) (zipTree ar br)

unzipTree :: BinaryTree (a, b) -> (BinaryTree a, BinaryTree b)
unzipTree EmptyTree = (EmptyTree, EmptyTree)
unzipTree (Branch (a, b) l r) =
  let (la, lb) = unzipTree l
      (ra, rb) = unzipTree r
   in (Branch a la ra, Branch b lb rb)

emptyTreeHeight :: Int
emptyTreeHeight = -1

wouldBeHeight :: Int -> Int -> Int
wouldBeHeight l r = 1 + (max l r)

heightTree :: BinaryTree a -> BinaryTree Int
heightTree EmptyTree = EmptyTree
heightTree (Branch _ l r) =
  let lt = heightTree l
      rt = heightTree r
      h =
        wouldBeHeight
          (fromMaybe emptyTreeHeight (node lt))
          (fromMaybe emptyTreeHeight (node rt))
   in Branch h lt rt

withHeight :: BinaryTree t -> BinaryTree (t, Int)
withHeight t = zipTree t (heightTree t)

readHeight :: BinaryTree (a, Int) -> Int
readHeight = fromMaybe emptyTreeHeight . readHeightMay
  where
    readHeightMay :: BinaryTree (a, Int) -> Maybe Int
    readHeightMay EmptyTree = Nothing
    readHeightMay (Branch (_, h) _ _) = Just h

branchWithHeight ::
     t -> BinaryTree (t, Int) -> BinaryTree (t, Int) -> BinaryTree (t, Int)
branchWithHeight n l r =
  Branch (n, wouldBeHeight (readHeight l) (readHeight r)) l r

leafWithHeight :: t -> BinaryTree (t, Int)
leafWithHeight n = branchWithHeight n EmptyTree EmptyTree

insertWithHeight :: (Ord a) => a -> BinaryTree (a, Int) -> BinaryTree (a, Int)
insertWithHeight a EmptyTree = leafWithHeight a
insertWithHeight a (Branch (n, h) l r) =
  if a < n
    then branchWithHeight n (insertWithHeight a l) r
    else branchWithHeight n l (insertWithHeight a r)

isSubtreeOf :: (Eq a) => BinaryTree a -> BinaryTree a -> Bool
isSubtreeOf EmptyTree _ = True
isSubtreeOf (Branch _ _ _) EmptyTree = False
isSubtreeOf a@(Branch an al ar) b@(Branch bn bl br) =
  an == bn && al `isSubtreeOf` bl && ar `isSubtreeOf` br ||
  a `isSubtreeOf` bl || a `isSubtreeOf` br

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
balance EmptyTree = SameHeight
balance (Branch _ EmptyTree EmptyTree) = SameHeight
balance (Branch _ lt@(Branch n l r) EmptyTree) =
  LeftTaller (BalanceFactor (readHeight lt - readHeight EmptyTree)) n l r
balance (Branch _ EmptyTree rt@(Branch n l r)) =
  RightTaller (BalanceFactor (readHeight EmptyTree - readHeight rt)) n l r
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

nodeAdmissable :: BinaryTree (a, Int) -> Bool
nodeAdmissable t = balanceFactor t `elem` [-1 .. 1]

isAVL :: (Ord a) => BinaryTree a -> Bool
isAVL t = go (withHeight t)
  where
    go EmptyTree = True
    go b@(Branch _ l r) = nodeAdmissable b && go l && go r

rotateLeft :: BinaryTree (a, Int) -> BinaryTree (a, Int)
rotateLeft EmptyTree = EmptyTree
rotateLeft b@(Branch _ _ EmptyTree) = b
rotateLeft (Branch (pn, _) pl (Branch (cn, _) cl cr)) =
  branchWithHeight cn (branchWithHeight pn pl cl) cr

rotateLeftMaybe :: BinaryTree (a, Int) -> Maybe (BinaryTree (a, Int))
rotateLeftMaybe EmptyTree = Nothing
rotateLeftMaybe (Branch _ _ EmptyTree) = Nothing
rotateLeftMaybe b = Just (rotateLeft b)

rotateRight :: BinaryTree (a, Int) -> BinaryTree (a, Int)
rotateRight EmptyTree = EmptyTree
rotateRight b@(Branch _ EmptyTree EmptyTree) = b
rotateRight b@(Branch _ EmptyTree Branch {}) = b
rotateRight p@(Branch (pn, _) c@(Branch (cn, _) cl cr) pr) =
  branchWithHeight cn cl (branchWithHeight pn cr pr)

rotateRightMaybe :: BinaryTree (a, Int) -> Maybe (BinaryTree (a, Int))
rotateRightMaybe EmptyTree = Nothing
rotateRightMaybe (Branch _ EmptyTree _) = Nothing
rotateRightMaybe b = Just (rotateRight b)

rebalanceOnce :: BinaryTree (a, Int) -> BinaryTree (a, Int)
rebalanceOnce EmptyTree = EmptyTree
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
insertWithHeightAVL a EmptyTree = leafWithHeight a
insertWithHeightAVL a (Branch (n, h) l r) =
  if a < n
    then rebalanceOnce (branchWithHeight n (insertWithHeightAVL a l) r)
    else rebalanceOnce (branchWithHeight n l (insertWithHeightAVL a r))

fromListAVL :: (Ord a) => [a] -> BinaryTree (a, Int)
fromListAVL = List.foldl' (flip insertWithHeightAVL) EmptyTree
