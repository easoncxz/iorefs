{-# LANGUAGE OverloadedLists #-}

module EightQueens where

import Control.Monad
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

data SearchState = SearchState
  { ssRows :: Seq Int
  , ssCols :: HashSet Int
  , ssDiagsAddition :: HashSet Int
  , ssDiagsSubtraction :: HashSet Int
  } deriving (Show, Eq)

initialState :: SearchState
initialState = SearchState [] [] [] []

oneStep :: Int -> SearchState -> [SearchState]
oneStep n ss@SearchState {..} = do
  col <- [0 .. n - 1]
  let row = Seq.length ssRows
      newRows = ssRows :|> col
      newCols = HashSet.insert col ssCols
      newDiagsAddition = HashSet.insert (row + col) ssDiagsAddition
      newDiagsSubtraction = HashSet.insert (row - col) ssDiagsSubtraction
  guard (HashSet.size newCols /= HashSet.size ssCols)
  guard (HashSet.size newDiagsAddition /= HashSet.size ssDiagsAddition)
  guard (HashSet.size newDiagsSubtraction /= HashSet.size ssDiagsSubtraction)
  return $
    ss
      { ssRows = newRows
      , ssCols = newCols
      , ssDiagsAddition = newDiagsAddition
      , ssDiagsSubtraction = newDiagsSubtraction
      }

solutionsFrom :: Int -> SearchState -> [SearchState]
solutionsFrom n ss@SearchState {..} =
  if Seq.length ssRows == n
    then [ss]
    else oneStep n ss >>= solutionsFrom n

eightQueens :: [Seq Int]
eightQueens = ssRows <$> solutionsFrom 8 initialState
