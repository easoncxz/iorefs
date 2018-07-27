module SearchTree.Benchmarking where

import SearchTree.AVLTree (AVLTree(..))
import qualified SearchTree.AVLTree as AVL
import SearchTree.Class (SearchTree)
import qualified SearchTree.Class as SearchTree

import Control.Concurrent
import Control.DeepSeq
import Data.Functor
import Data.Int
import Gauge.Main
import System.Random
import Test.QuickCheck (arbitrary, generate, vectorOf)

type Element = Int16

setupInsert :: Int -> IO (Element, AVLTree Element)
setupInsert n =
  generate $ do
    c <- arbitrary
    tree <- SearchTree.fromList <$> vectorOf n arbitrary
    return (c, force tree)

benchInsertOnSize :: Int -> Benchmark
benchInsertOnSize n =
  bench ("inserting into a " ++ show n ++ "-element tree") $
  perRunEnv (setupInsert n) (\(~(x, t)) -> return (SearchTree.insert x t))

benchMain :: IO ()
benchMain = defaultMain [bgroup "AVLTree" [benchInsertOnSize n | n <- [100,200 .. 1000]]]
