module Test.Relude where

import Relude

prop_reverseList :: [Int] -> Bool
prop_reverseList xs = reverseList xs == reverse xs

prop_deleteAll :: Int -> [Int] -> Bool
prop_deleteAll v xs = deleteAll v xs == filter (/= v) xs

prop_listAppend :: [Int] -> [Int] -> Bool
prop_listAppend xs ys = xs ++ ys == xs `listAppend` ys

prop_listAppend2 :: [Int] -> [Int] -> Bool
prop_listAppend2 xs ys = xs ++ ys == xs `listAppend2` ys

prop_myFoldL :: [String] -> Bool
prop_myFoldL xs = myFoldL tell "_" xs == foldl tell "_" xs

prop_myFoldR :: [String] -> Bool
prop_myFoldR xs = myFoldR tell "_" xs == foldr tell "_" xs

prop_diffConcat :: [[String]] -> Bool
prop_diffConcat ss = diffConcat ss == concat ss


