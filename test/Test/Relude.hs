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
prop_myFoldL xs = myFoldL tells id (map (++) xs) "_" == foldl tells id (map (++) xs) "_"

prop_myFoldR :: [String] -> Bool
prop_myFoldR xs = myFoldR tells id (map (++) xs) "_" == foldr tells id (map (++) xs) "_"

prop_diffConcat :: [[Int]] -> Bool
prop_diffConcat ss = diffConcat ss == concat ss
