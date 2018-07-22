module Origami where

import Control.Arrow ((<<<), (>>>))
import qualified Data.List as List

heightList :: [a] -> [Int]
heightList = snd . List.foldr one (0, [])
  where
    one :: a -> (Int, [Int]) -> (Int, [Int])
    one _ (n, ns) = (n + 1, (n + 1) : ns)

foldlViaFoldr :: (z -> a -> z) -> z -> [a] -> z
foldlViaFoldr (/) z xs = foldr (>>>) id [(/ x) | x <- xs] z

foldrViaFoldl :: (a -> z -> z) -> z -> [a] -> z
foldrViaFoldl (^) z xs = foldl (<<<) id [(x ^) | x <- xs] z
