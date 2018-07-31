module Bisection where

import Control.Monad.State
import Data.Functor
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

bisectFind :: (Ord a, G.Vector v a, MonadState Int m) => a -> v a -> m (Maybe a)
bisectFind n xs = do
  modify (+ 1)
  case G.length xs of
    0 -> return Nothing
    len -> do
      let midpos = floor (fromIntegral len / 2)
          midval = xs G.! midpos
      case compare n midval of
        LT -> bisectFind n (G.slice 0 midpos xs)
        EQ -> return (Just midval)
        GT -> bisectFind n (G.slice (midpos + 1) (len - (midpos + 1)) xs)

bisectFind' :: (Ord a) => a -> [a] -> Maybe a
bisectFind' n xs =
  let vec = V.fromList (List.sort xs)
   in fst (runState (bisectFind n vec) 0)
