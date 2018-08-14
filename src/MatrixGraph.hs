module MatrixGraph where

import FingerQueue (FingerQueue)
import qualified FingerQueue

import Control.Arrow (second)
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Ix
import qualified Data.List as List
import Data.Vector (Vector)
import qualified Data.Vector as Vector

newtype MatrixGraph node edge = MatrixGraph
  { getMatrixGraph :: Array (node, node) (Maybe edge)
  } deriving (Show)

fromNodesAndEdges ::
     (Ix node, Enum node) => (node, node) -> [((node, node), edge)] -> MatrixGraph node edge
fromNodesAndEdges (nodeFrom, nodeTo) edges =
  let emptyGraph =
        Array.array
          ((nodeFrom, nodeFrom), (nodeTo, nodeTo))
          [((f, t), Nothing) | f <- [nodeFrom .. nodeTo], t <- [nodeFrom .. nodeTo]]
   in MatrixGraph (emptyGraph Array.// [(nn, Just e) | (nn, e) <- edges])

nodesFrom :: MatrixGraph node edge -> node -> [node]
nodesFrom graph node = undefined

data TodoOps c e = TodoOps
  { todoAdd :: e -> c e -> c e
  , todoDel :: c e -> Maybe (e, c e)
  }

todoQueueOps :: TodoOps FingerQueue node
todoQueueOps =
  TodoOps
    { todoAdd = FingerQueue.enqueue
    , todoDel =
        \q -> do
          e <- FingerQueue.front q
          q' <- FingerQueue.dequeue q
          return (e, q')
    }

todoStackOps :: TodoOps [] node
todoStackOps = TodoOps {todoAdd = (:), todoDel = List.uncons}
