module SearchTree.Trie where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)

newtype Trie a = Trie
  { unTrie :: HashMap (Maybe a) (Trie a)
  } deriving (Show, Eq)

empty :: Trie a
empty = Trie HM.empty

insert :: (Eq a, Hashable a) => [a] -> Trie a -> Trie a
insert [] (Trie m) = Trie (HM.insert Nothing empty m)
insert (x:xs) (Trie m) =
  let insertXs mbSubtrie = Just (insert xs (fromMaybe empty mbSubtrie))
   in Trie (HM.alter insertXs (Just x) m)

member :: (Eq a, Hashable a) => [a] -> Trie a -> Bool
member [] (Trie m) = HM.member Nothing m
member (x:xs) (Trie m) = member xs (HM.lookupDefault empty (Just x) m)

size :: Trie a -> Int
size (Trie m) = undefined

