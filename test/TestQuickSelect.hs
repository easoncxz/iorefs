module TestQuickSelect
  ( tests
  ) where

import qualified System.Random as Random
import Test.QuickCheck

import QuickSelect

tests :: IO ()
tests = do
  quickCheck prop_selectSmall
  quickCheck prop_partition
