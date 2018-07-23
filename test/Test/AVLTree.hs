module Test.AVLTree where

import AVLTree
import BinaryTree (BinaryTree(Branch, Empty))
import qualified BinaryTree as BT

import Test.BinaryTree (searchProperty)

import Control.Arrow (first, second)
import Data.Maybe (fromMaybe)

prop_withHeightZipVsFold :: BinaryTree Char -> Bool
prop_withHeightZipVsFold t = BT.zipTree t (heightTree t) == treeWithHeight t

prop_insertCommutesWithHeight :: Char -> BinaryTree Char -> Bool
prop_insertCommutesWithHeight x tree =
  treeWithHeight (BT.insert x tree) == insertWithHeight x (treeWithHeight tree)

prop_deleteCommutesWithHeight :: Char -> BinaryTree Char -> Bool
prop_deleteCommutesWithHeight x tree =
  fmap treeWithHeight (BT.delete x tree) == deleteWithHeight x (treeWithHeight tree)

prop_popHeadCommutesWithHeight :: BinaryTree Char -> Bool
prop_popHeadCommutesWithHeight tree =
  fmap (second treeWithHeight) (BT.popHead tree) ==
  fmap (first fst) (popHeadWithHeight (treeWithHeight tree))

prop_popHeadWithHeightAVLPreservesAVL :: AVLTree Char -> Bool
prop_popHeadWithHeightAVLPreservesAVL (AVLTree t) =
  case popHeadWithHeightAVL t of
    Nothing -> True
    Just (_, t') -> isAVL (treeWithoutHeight t')

prop_popLastCommutesWithHeight :: BinaryTree Char -> Bool
prop_popLastCommutesWithHeight tree =
  fmap (first treeWithHeight) (BT.popLast tree) ==
  fmap (second fst) (popLastWithHeight (treeWithHeight tree))

prop_popLastWithHeightAVLPreservesAVL :: AVLTree Char -> Bool
prop_popLastWithHeightAVLPreservesAVL (AVLTree t) =
  case popLastWithHeightAVL t of
    Nothing -> True
    Just (t', _) -> isAVL (treeWithoutHeight t')

prop_rotateLeftPreservesSearchProperty :: BinaryTree Char -> Bool
prop_rotateLeftPreservesSearchProperty t =
  searchProperty . treeWithoutHeight . rotateLeft . treeWithHeight $ t

prop_rotateRightPreservesSearchProperty :: BinaryTree Char -> Bool
prop_rotateRightPreservesSearchProperty t =
  searchProperty . treeWithoutHeight . rotateRight . treeWithHeight $ t

prop_rotateBackAndForthAgain :: BinaryTree Char -> Bool
prop_rotateBackAndForthAgain t =
  let th = treeWithHeight t
   in fromMaybe th (rotateLeftMaybe =<< rotateRightMaybe th) == th &&
      fromMaybe th (rotateRightMaybe =<< rotateLeftMaybe th) == th

prop_avlTreesAreAVL :: AVLTree Char -> Bool
prop_avlTreesAreAVL (AVLTree t) = isAVL t

prop_insertWithHeightAVLPreservesSearchProperty :: Char -> AVLTree Char -> Bool
prop_insertWithHeightAVLPreservesSearchProperty x (AVLTree t) =
  searchProperty . treeWithoutHeight . insertWithHeightAVL x $ t

prop_insertWithHeightAVLPreservesHeightInvariant :: Char -> AVLTree Char -> Bool
prop_insertWithHeightAVLPreservesHeightInvariant x (AVLTree t) =
  let large = insertWithHeightAVL x t
   in large == treeWithNewHeight large

prop_insertWithHeightAVLPreservesAVLProperty :: Char -> AVLTree Char -> Bool
prop_insertWithHeightAVLPreservesAVLProperty x (AVLTree t) =
  isAVL . treeWithoutHeight . insertWithHeightAVL x $ t

prop_deleteWithHeightAVLPreservesSearchProperty :: Char -> AVLTree Char -> Bool
prop_deleteWithHeightAVLPreservesSearchProperty x (AVLTree t) =
  fromMaybe True . fmap (searchProperty . treeWithoutHeight) $ deleteWithHeightAVL x t

prop_deleteWithHeightAVLPreservesHeightInvariant :: Char -> AVLTree Char -> Bool
prop_deleteWithHeightAVLPreservesHeightInvariant x (AVLTree t) =
  let largeM = deleteWithHeightAVL x t
   in largeM == fmap treeWithNewHeight largeM

prop_deleteWithHeightAVLPreservesAVLProperty :: Char -> AVLTree Char -> Bool
prop_deleteWithHeightAVLPreservesAVLProperty x (AVLTree t) =
  fromMaybe True . fmap (isAVL . treeWithoutHeight) $ deleteWithHeightAVL x t

badTree :: AVLTree Char
badTree =
  AVLTree
    { runAVLTree =
        Branch
          ('R', 6)
          (Branch
             ('\'', 5)
             (Branch
                ('\n', 4)
                (Branch
                   ('\a', 2)
                   (Branch
                      ('\ENQ', 1)
                      (Branch ('\ETX', 0) Empty Empty)
                      (Branch ('\ENQ', 0) Empty Empty))
                   (Branch ('\b', 0) Empty Empty))
                (Branch
                   ('\DC1', 3)
                   (Branch
                      ('\SI', 1)
                      (Branch ('\SO', 0) Empty Empty)
                      (Branch ('\DLE', 0) Empty Empty))
                   (Branch
                      ('\CAN', 2)
                      (Branch ('\NAK', 1) (Branch ('\DC1', 0) Empty Empty) Empty)
                      (Branch
                         ('\US', 1)
                         (Branch ('\EM', 0) Empty Empty)
                         (Branch (' ', 0) Empty Empty)))))
             (Branch
                ('E', 4)
                (Branch
                   ('6', 3)
                   (Branch
                      (',', 2)
                      (Branch ('*', 1) (Branch ('(', 0) Empty Empty) Empty)
                      (Branch ('6', 0) Empty Empty))
                   (Branch
                      ('B', 2)
                      (Branch ('9', 1) (Branch ('7', 0) Empty Empty) (Branch ('>', 0) Empty Empty))
                      (Branch ('D', 0) Empty Empty)))
                (Branch
                   ('J', 3)
                   (Branch
                      ('G', 2)
                      (Branch ('F', 1) Empty (Branch ('F', 0) Empty Empty))
                      (Branch ('I', 1) Empty (Branch ('I', 0) Empty Empty)))
                   (Branch ('L', 1) (Branch ('L', 0) Empty Empty) (Branch ('M', 0) Empty Empty)))))
          (Branch
             ('\50524', 5)
             (Branch
                ('o', 4)
                (Branch
                   ('e', 3)
                   (Branch ('R', 1) Empty (Branch ('T', 0) Empty Empty))
                   (Branch
                      ('g', 2)
                      (Branch ('g', 0) Empty Empty)
                      (Branch ('g', 1) Empty (Branch ('h', 0) Empty Empty))))
                (Branch
                   ('z', 2)
                   (Branch ('w', 1) (Branch ('p', 0) Empty Empty) (Branch ('y', 0) Empty Empty))
                   (Branch ('\41687', 1) (Branch ('\DEL', 0) Empty Empty) Empty)))
             (Branch
                ('\625117', 4)
                (Branch
                   ('\436591', 3)
                   (Branch
                      ('\292061', 2)
                      (Branch ('\266047', 1) (Branch ('\236332', 0) Empty Empty) Empty)
                      (Branch ('\385672', 0) Empty Empty))
                   (Branch
                      ('\463172', 2)
                      (Branch ('\455300', 0) Empty Empty)
                      (Branch ('\591369', 1) (Branch ('\497715', 0) Empty Empty) Empty)))
                (Branch
                   ('\1079950', 3)
                   (Branch
                      ('\923195', 2)
                      (Branch
                         ('\732548', 1)
                         (Branch ('\727662', 0) Empty Empty)
                         (Branch ('\824778', 0) Empty Empty))
                      (Branch ('\1068584', 0) Empty Empty))
                   (Branch
                      ('\1094321', 1)
                      (Branch ('\1082926', 0) Empty Empty)
                      (Branch ('\1111278', 0) Empty Empty)))))
    }

badTreeAfter :: BinaryTree (Char, Int)
badTreeAfter =
  Branch
    ('R', 6)
    (Branch
       ('\'', 5)
       (Branch
          ('\n', 4)
          (Branch
             ('\a', 2)
             (Branch ('\ENQ', 1) (Branch ('\ETX', 0) Empty Empty) (Branch ('\ENQ', 0) Empty Empty))
             (Branch ('\b', 0) Empty Empty))
          (Branch
             ('\DC1', 3)
             (Branch ('\SI', 1) (Branch ('\SO', 0) Empty Empty) (Branch ('\DLE', 0) Empty Empty))
             (Branch
                ('\CAN', 2)
                (Branch ('\NAK', 1) (Branch ('\DC1', 0) Empty Empty) Empty)
                (Branch ('\US', 1) (Branch ('\EM', 0) Empty Empty) (Branch (' ', 0) Empty Empty)))))
       (Branch
          ('E', 4)
          (Branch
             ('6', 3)
             (Branch
                (',', 2)
                (Branch ('*', 1) (Branch ('(', 0) Empty Empty) Empty)
                (Branch ('6', 0) Empty Empty))
             (Branch
                ('B', 2)
                (Branch ('9', 1) (Branch ('7', 0) Empty Empty) (Branch ('>', 0) Empty Empty))
                (Branch ('D', 0) Empty Empty)))
          (Branch
             ('J', 3)
             (Branch
                ('G', 2)
                (Branch ('F', 1) Empty (Branch ('F', 0) Empty Empty))
                (Branch ('I', 1) Empty (Branch ('I', 0) Empty Empty)))
             (Branch ('L', 1) (Branch ('L', 0) Empty Empty) (Branch ('M', 0) Empty Empty)))))
    (Branch
       ('\50524', 5)
       (Branch
          ('o', 4)
          (Branch
             ('e', 3)
             (Branch ('T', 0) Empty Empty)
             (Branch
                ('g', 2)
                (Branch ('g', 0) Empty Empty)
                (Branch ('g', 1) Empty (Branch ('h', 0) Empty Empty))))
          (Branch
             ('z', 2)
             (Branch ('w', 1) (Branch ('p', 0) Empty Empty) (Branch ('y', 0) Empty Empty))
             (Branch ('\41687', 1) (Branch ('\DEL', 0) Empty Empty) Empty)))
       (Branch
          ('\625117', 4)
          (Branch
             ('\436591', 3)
             (Branch
                ('\292061', 2)
                (Branch ('\266047', 1) (Branch ('\236332', 0) Empty Empty) Empty)
                (Branch ('\385672', 0) Empty Empty))
             (Branch
                ('\463172', 2)
                (Branch ('\455300', 0) Empty Empty)
                (Branch ('\591369', 1) (Branch ('\497715', 0) Empty Empty) Empty)))
          (Branch
             ('\1079950', 3)
             (Branch
                ('\923195', 2)
                (Branch
                   ('\732548', 1)
                   (Branch ('\727662', 0) Empty Empty)
                   (Branch ('\824778', 0) Empty Empty))
                (Branch ('\1068584', 0) Empty Empty))
             (Branch
                ('\1094321', 1)
                (Branch ('\1082926', 0) Empty Empty)
                (Branch ('\1111278', 0) Empty Empty)))))
