module SearchTree.Trie where

import Data.Foldable
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

newtype Trie a = Trie
  { unTrie :: Map (Maybe a) (Trie a)
  } deriving (Show, Eq)

empty :: Trie a
empty = Trie M.empty

null :: Trie a -> Bool
null = M.null . unTrie

nonNull :: Trie a -> Maybe (Trie a)
nonNull (Trie m)
  | M.null m = Nothing
  | otherwise = Just (Trie m)

insert :: (Ord a) => [a] -> Trie a -> Trie a
insert [] (Trie m) = Trie (M.insert Nothing empty m)
insert (c:cs) (Trie m) =
  let insertXs mbSubtrie = Just (insert cs (fromMaybe empty mbSubtrie))
   in Trie (M.alter insertXs (Just c) m)

member :: (Ord a) => [a] -> Trie a -> Bool
member [] (Trie m) = M.member Nothing m
member (c:cs) (Trie m) = fromMaybe False (member cs <$> M.lookup (Just c) m)

delete :: (Ord a) => [a] -> Trie a -> Maybe (Trie a)
delete [] (Trie m) = M.lookup Nothing m $> Trie (M.delete Nothing m)
delete (c:cs) (Trie m) = do
  subtrie <- M.lookup (Just c) m
  newSubtrie <- delete cs subtrie
  return (Trie (M.update (const (nonNull newSubtrie)) (Just c) m))

delete' :: (Ord a) => [a] -> Trie a -> Trie a
delete' [] (Trie m) = Trie (M.delete Nothing m)
delete' (c:cs) (Trie m) = Trie (M.update (nonNull . delete' cs) (Just c) m)

size :: Trie a -> Int
size (Trie m) = M.foldlWithKey sizeUnderChar 0 m
  where
    sizeUnderChar s c t =
      case c of
        Nothing -> 1
        Just _ -> s + size t

fromList :: (Ord a) => [[a]] -> Trie a
fromList = foldr insert empty
