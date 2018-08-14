module MatrixGraph where

import FingerQueue (FingerQueue)
import qualified FingerQueue

import Control.Arrow (second)
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Ix
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector

newtype MatrixGraph node edge = MatrixGraph
  { getMatrixGraph :: Array (node, node) (Maybe edge)
  } deriving (Show)

type IsNode node = (Ix node, Enum node)

fromNodesAndEdges :: IsNode node => (node, node) -> [((node, node), edge)] -> MatrixGraph node edge
fromNodesAndEdges (nodeFrom, nodeTo) edges =
  let emptyGraph =
        Array.array
          ((nodeFrom, nodeFrom), (nodeTo, nodeTo))
          [((f, t), Nothing) | f <- [nodeFrom .. nodeTo], t <- [nodeFrom .. nodeTo]]
   in MatrixGraph (emptyGraph Array.// [(nn, Just e) | (nn, e) <- edges])

nodesFrom :: (Ix node, Enum node) => MatrixGraph node edge -> node -> [node]
nodesFrom (MatrixGraph arr) node =
  let ((nodeFrom, _), (nodeTo, _)) = Array.bounds arr
   in [n | n <- [nodeFrom .. nodeTo], isJust (arr Array.! (node, n))]

data TodoOps c e = TodoOps
  { todoAdd :: e -> c e -> c e
  , todoDel :: c e -> Maybe (e, c e)
  , todoEmpty :: c e
  }

todoQueueOps :: TodoOps FingerQueue node
todoQueueOps =
  TodoOps
    { todoAdd = FingerQueue.enqueue
    , todoDel = \q -> (,) <$> FingerQueue.front q <*> FingerQueue.dequeue q
    , todoEmpty = FingerQueue.empty
    }

todoStackOps :: TodoOps [] node
todoStackOps = TodoOps {todoAdd = (:), todoDel = List.uncons, todoEmpty = []}

search' ::
     forall node edge todo. IsNode node
  => TodoOps todo node
  -> MatrixGraph node edge
  -> node
  -> [node]
search' TodoOps {..} graph@(MatrixGraph arr) start =
  let iter :: Set node -> todo node -> [node] -> [node]
      iter seen todo output =
        case todoDel todo of
          Nothing -> reverse output
          Just (curr, didOne) ->
            let seeOneMore = Set.insert curr seen
             in if Set.size seeOneMore > Set.size seen
                  then let moreTodo = List.foldl' (flip todoAdd) didOne (nodesFrom graph curr)
                        in iter seeOneMore moreTodo (curr : output)
                  else iter seen didOne output
   in iter Set.empty (todoAdd start todoEmpty) []

search ::
     forall node edge todo. IsNode node
  => TodoOps todo node
  -> MatrixGraph node edge
  -> node
  -> [node]
search TodoOps {..} graph@(MatrixGraph arr) start = rec Set.empty (todoAdd start todoEmpty)
  where
    rec :: Set node -> todo node -> [node]
    rec seen todo =
      case todoDel todo of
        Nothing -> []
        Just (curr, didOne) ->
          let seeOneMore = Set.insert curr seen
           in if Set.size seeOneMore > Set.size seen
                then let moreTodo = List.foldl' (flip todoAdd) didOne (nodesFrom graph curr)
                      in curr : rec seeOneMore moreTodo
                else rec seen didOne

depthFirstSearch :: IsNode node => MatrixGraph node edge -> node -> [node]
depthFirstSearch = search todoStackOps

breadthFirstSearch :: IsNode node => MatrixGraph node edge -> node -> [node]
breadthFirstSearch = search todoQueueOps
