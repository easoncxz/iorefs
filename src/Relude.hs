module Relude where

reverseList :: [a] -> [a]
reverseList = go []
  where
    go rs [] = rs
    go rs (x:xs) = go (x : rs) xs

-- | Delete all occurrences of v in xs
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll = go []
  where
    go acc _ [] = reverse acc
    go acc v (x:xs) =
      if v == x
        then go acc v xs
        else go (x : acc) v xs

listAppend :: [a] -> [a] -> [a]
listAppend [] ys = ys
listAppend (x:xs) ys = x : (xs ++ ys)

listAppend2 :: [a] -> [a] -> [a]
listAppend2 = go id
  where
    go z [] ss = z ss
    go z (x:xs) ss = go (z . (x :)) xs ss

-- | Concatenation, using "difference lists"
diffConcat :: [[a]] -> [a]
diffConcat xs = foldr (.) id (fmap (++) xs) []

tell :: String -> String -> String
tell a b = "(" ++ a ++ " . " ++ b ++ ")"

tells :: ShowS -> ShowS -> ShowS
tells a b = shows "(" . a . shows " . " . b . shows ")"

myFoldL :: (b -> a -> b) -> b -> [a] -> b
myFoldL f z [] = z
myFoldL f z (x:xs) = myFoldL f (f z x) xs

myFoldR :: (a -> b -> b) -> b -> [a] -> b
myFoldR f z [] = z
myFoldR f z (x:xs) = f x (myFoldR f z xs)

myScanL :: (b -> a -> b) -> b -> [a] -> [b]
myScanL f z xs = z : go f z xs
  where
    go f _ [] = []
    go f z (x:xs) =
      let z' = f z x
       in z' : go f z' xs

myScanL' :: (b -> a -> b) -> b -> [a] -> [b]
myScanL' f z xs = z : go f z xs
  where
    go f _ [] = []
    go f z (x:xs) =
      let z' = f z x
       in z' `seq` (z' : go f z' xs)

prop_myScanL :: (Char -> Int -> Char) -> Char -> [Int] -> Bool
prop_myScanL f z xs = myScanL f z xs == scanl f z xs
