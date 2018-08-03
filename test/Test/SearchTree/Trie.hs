module Test.SearchTree.Trie where

import SearchTree.Trie (Trie)
import qualified SearchTree.Trie as Trie

import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck

prop_insertIsIdempotent :: String -> Set String -> Bool
prop_insertIsIdempotent s ss =
  let t = Trie.fromList (toList ss)
   in Trie.insert s t == Trie.insert s (Trie.insert s t)

prop_insertPreservesCount :: Set String -> Bool
prop_insertPreservesCount ss =
  let t = Trie.fromList (toList ss)
   in Trie.size t == Set.size ss

prop_deleteDeletesOneElement :: String -> Set String -> Bool
prop_deleteDeletesOneElement s ss =
  let t = Trie.fromList (toList ss)
   in Set.member s ss == Trie.member s t

prop_insertThenDelete :: String -> Bool
prop_insertThenDelete s =
  Trie.delete s (Trie.insert s Trie.empty) == Just Trie.empty

prop_cannotDeleteTwice :: Set String -> Property
prop_cannotDeleteTwice ss =
  not (Set.null ss) ==> do
    s <- elements (toList ss)
    let t = Trie.fromList (toList ss)
    return $
      case Trie.delete s t of
        Nothing -> False
        Just t' -> Trie.delete s t' == Nothing

prop_insertThenDelete' :: String -> Bool
prop_insertThenDelete' s =
  Trie.delete' s (Trie.insert s Trie.empty) == Trie.empty

prop_deleteIsIdempotent :: String -> Set String -> Bool
prop_deleteIsIdempotent s ss =
  let t = Trie.fromList (toList ss)
   in Trie.delete' s t == Trie.delete' s (Trie.delete' s t)
