module Test.SearchTree where

import SearchTree.Class (SearchTree)
import qualified SearchTree.Class as SearchTree

import Control.Arrow (first, second)
import Data.Foldable
import qualified Data.List as List
import Data.Proxy
import qualified Data.Sequence as Seq
import Safe (headMay, lastMay)

import Test.QuickCheck
  ( Arbitrary(arbitrary)
  , Gen
  , Property
  , Testable(property)
  , conjoin
  , counterexample
  , verbose
  )

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs =
  case Seq.fromList xs of
    Seq.Empty -> Nothing
    init Seq.:|> last -> Just (toList init, last)

contract ::
     forall tree el.
     (Ord el, Show el, Show (tree el), Arbitrary el, Arbitrary (tree el), SearchTree tree)
  => Proxy tree
  -> Proxy el
  -> Property
contract _ _ =
  let treeBool = counterexample :: String -> (tree el -> Bool) -> Property
   in conjoin
        [ treeBool "searchProperty" $ \t -> toList t == List.sort (toList t)
        , counterexample "elemMatchesListElem" $ \(x :: el) (xs :: [el]) ->
            let t = SearchTree.fromList xs :: tree el
             in SearchTree.elem x t == List.elem x xs
        , treeBool "headMatchesToList" $ \t -> SearchTree.head t == headMay (toList t)
        , treeBool "lastMatchesToList" $ \t -> SearchTree.last t == lastMay (toList t)
        , treeBool "popHeadMatchesToList" $ \t ->
            fmap (second toList) (SearchTree.popHead t) == List.uncons (toList t)
        , treeBool "popLastMatchesToList" $ \t ->
            fmap (first toList) (SearchTree.popLast t) == unsnoc (toList t)
        , counterexample "insertMaintainsSearchProperty" $ \(x :: el) (t :: tree el) ->
            let inorder = toList (SearchTree.insert x t)
             in inorder == List.sort inorder
        , counterexample "deleteMaintainsSearchProperty" $ \(x :: el) (t :: tree el) ->
            case (x `SearchTree.elem` t, SearchTree.delete x t) of
              (False, Nothing) -> True
              (True, Just smaller) -> toList smaller == List.sort (toList smaller)
              _ -> False
        ]
