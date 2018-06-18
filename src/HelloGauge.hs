module HelloGauge where

import Data.Bits (shiftL)

import Gauge.Main
import Gauge.Main.Options

fact :: Integer -> Integer
fact n =
  if n > 0
    then product [1 .. n]
    else 1

appendN :: Char -> Int -> [Char]
appendN c 0 = []
appendN c n = appendN c (n - 1) ++ [c]

prependN :: Char -> Int -> [Char]
prependN c 0 = []
prependN c n = c : (prependN c (n - 1))

benchMain :: IO ()
benchMain = do
  defaultMain
    [ bgroup
        "appendN"
        [bench (show n) (whnf (appendN '@') n) | n <- [10000,20000 .. 100000]]
    , bgroup
        "prependN"
        [bench (show n) (whnf (prependN '@') n) | n <- [10000,20000 .. 100000]]
    ]
