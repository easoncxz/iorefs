module Arrays where

import Control.Monad
import Data.Array.IArray

type Grid = Array (Int, Int) Char

grid :: Grid
grid =
  let rowCount = 30
      colCount = 5
   in array
        ((1, 1), (rowCount, colCount))
        [((r, c), '*') | r <- [1 .. rowCount], c <- [1 .. colCount]]

putGridLn :: Grid -> IO ()
putGridLn g = do
  let (_, (rowCount, colCount)) = bounds g
  forM_ [1 .. rowCount] $ \row -> do
    forM_ [1 .. colCount] $ \col -> putChar (g ! (row, col))
    putChar '\n'
