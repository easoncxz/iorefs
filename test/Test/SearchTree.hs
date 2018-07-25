module Test.SearchTree where

import SearchTree.Class (SearchTree)
import qualified SearchTree.Class as SearchTree

import Data.Foldable
import Data.Proxy
import Safe (headMay, lastMay)

import Test.QuickCheck (Arbitrary(arbitrary), Gen, Property, Testable(property), conjoin)

contract ::
     forall tree a. (Ord a, Show (tree a), Arbitrary (tree a), SearchTree tree)
  => Proxy tree
  -> Proxy a
  -> Property
contract _ _ = conjoin [property headMatchesToList, property lastMatchesToList]
  where
    headMatchesToList :: tree a -> Bool
    headMatchesToList t = SearchTree.head t == headMay (toList t)
    lastMatchesToList :: tree a -> Bool
    lastMatchesToList t = SearchTree.last t == lastMay (toList t)
