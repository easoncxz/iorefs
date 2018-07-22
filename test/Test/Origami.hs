module Test.Origami where

import Origami

import Data.Char (ord)
import qualified Data.List as List

prop_heightList :: [Char] -> Bool
prop_heightList cs = heightList cs == reverse (fst <$> zip [1 ..] cs)

prop_foldlViaFoldr :: [Char] -> Bool
prop_foldlViaFoldr cs =
  let f z a = z + ord a
      myFoldl = foldlViaFoldr f 0 cs
      goodFoldl = List.foldl f 0 cs
   in myFoldl == goodFoldl

prop_foldrViaFoldl :: [Char] -> Bool
prop_foldrViaFoldl cs =
  let f a z = z + ord a
      myFoldr = foldrViaFoldl f 0 cs
      goodFoldr = List.foldr f 0 cs
   in myFoldr == goodFoldr
