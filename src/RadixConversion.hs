module RadixConversion
  ( Base(getBase)
  , base
  , unsafeBase
  , interpretWithSign
  , representWithSign
  ) where

import EuclideanDivision (divModEuclidean)

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Test.QuickCheck (Arbitrary(arbitrary), elements)

newtype Base = Base
  { getBase :: Int
  } deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

alphabet :: [Char]
alphabet = ['0' .. '9'] ++ ['a' .. 'z']

alphabetSize :: Int
alphabetSize = length alphabet

base :: Int -> Maybe Base
base n
  | 1 < abs n && abs n < alphabetSize = Just (Base n)
  | otherwise = Nothing

unsafeBase :: Int -> Base
unsafeBase = Base

instance Arbitrary Base where
  arbitrary = do
    b <- elements [-alphabetSize .. alphabetSize]
    case base b of
      Just good -> return good
      Nothing -> arbitrary

digitValue :: Map Char Int
digitValue = Map.fromList (zip alphabet [0 ..])

interpretWithSign :: Base -> String -> Maybe Int
interpretWithSign b ('-':ds) = negate <$> interpretWithSign b ds
interpretWithSign b ds = interpretReversed b (reverse ds)

interpretReversed :: Base -> String -> Maybe Int
interpretReversed _ [] = Just 0
interpretReversed (Base base) (d:ds) = do
  one <- Map.lookup d digitValue
  guard (one < abs base)
  higher <- interpretReversed (Base base) ds
  return (base * higher + one)

representWithSign :: Base -> Int -> String
representWithSign b n =
  if b > 0 && n < 0
    then '-' : representWithSign b (-n)
    else if n == 0
           then "0"
           else reverse (representReversed b n)

representReversed :: Base -> Int -> String
representReversed _ 0 = ""
representReversed (Base base) n =
  let (higher, one) = divModEuclidean n base
   in (alphabet !! one) : representReversed (Base base) higher
