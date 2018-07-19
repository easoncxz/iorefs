module QuickSelect where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.STRef
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Debug.Trace as Debug
import Protolude.Safe (atMay, headMay)

checkIndexOrderingInvariant :: STRef s Int -> STRef s Int -> Int -> ST s ()
checkIndexOrderingInvariant loPtr hiPtr len = do
  lo <- readSTRef loPtr
  hi <- readSTRef hiPtr
  if not (1 <= lo && lo <= hi + 1 && hi + 1 <= len)
    then error $ "checkIndexOrderingInvariant violated: " ++ show (lo, hi, len)
    else return ()

checkPartitionConditionInvariant ::
     (G.Vector v a, Ord a, Show a) => STRef s Int -> STRef s Int -> v a -> ST s ()
checkPartitionConditionInvariant loPtr hiPtr xs = do
  lo <- readSTRef loPtr
  hi <- readSTRef hiPtr
  let len = G.length xs
      pivot = xs G.! 0
      smalls = G.slice 1 (lo - 1) xs
      larges = G.slice (hi + 1) (len - hi - 1) xs
  if not (G.all (< pivot) smalls)
    then error $ "checkPartitionConditionInvariant failed on smalls: " ++ show (lo, hi, G.toList xs)
    else if not (G.all (>= pivot) larges)
           then error $
                "checkPartitionConditionInvariant failed on larges: " ++ show (lo, hi, G.toList xs)
           else return ()

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ condM action = do
  cond <- condM
  if cond
    then do
      _ <- action
      whileM_ condM action
    else return ()

partition :: (Show a, U.Unbox a, Ord a) => UM.MVector s a -> ST s (Maybe Int)
partition xs = do
  case GM.length xs of
    0 -> return Nothing
    -- 1 -> return (Just 0)
    len ->
      Just <$> do
        pivotElem <- GM.read xs 0
        loPtr <- newSTRef 1
        hiPtr <- newSTRef (len - 1)
        checkInvariants loPtr hiPtr xs
        whileM_ ((<=) <$> readSTRef loPtr <*> readSTRef hiPtr) $ do
          loIx <- readSTRef loPtr
          hiIx <- readSTRef hiPtr
          loElem <- GM.read xs loIx
          hiElem <- GM.read xs hiIx
          case (loElem < pivotElem, pivotElem <= hiElem) of
            (True, True) -> do
              modifySTRef loPtr (+ 1)
              modifySTRef hiPtr (subtract 1)
            (True, False) -> modifySTRef loPtr (+ 1)
            (False, True) -> modifySTRef hiPtr (subtract 1)
            (False, False) -> do
              GM.swap xs loIx hiIx
              modifySTRef loPtr (+ 1)
              modifySTRef hiPtr (subtract 1)
          checkInvariants loPtr hiPtr xs
        pivotIx <- subtract 1 <$> readSTRef loPtr
        GM.swap xs 0 pivotIx
        return pivotIx
  where
    checkInvariants loPtr hiPtr xs = do
      checkIndexOrderingInvariant loPtr hiPtr (GM.length xs)
      checkPartitionConditionInvariant loPtr hiPtr =<< U.freeze xs -- problematic with generic `G.freeze`

partition' :: (Show a, Ord a, U.Unbox a) => [a] -> Maybe (Int, [a])
partition' xs =
  runST $ do
    mut <- U.thaw (U.fromList xs)
    mbPivotIx <- partition mut
    case mbPivotIx of
      Nothing -> return Nothing
      Just pivotIx -> do
        frozen <- U.toList <$> U.freeze mut
        return (Just (pivotIx, frozen))

selectSmallST :: (Show a, Ord a, U.Unbox a) => Int -> UM.MVector s a -> ST s (Maybe a)
selectSmallST n xs
  | n < 0 || UM.length xs <= n = return Nothing
selectSmallST n xs = do
  frozen <- U.freeze xs
  _ <- return $ Debug.trace (show (n, frozen)) ()
  mbPivotIx <- partition xs
  case mbPivotIx of
    Nothing -> return Nothing
    Just pivotIx ->
      case compare n pivotIx of
        LT -> selectSmallST n (UM.slice 0 pivotIx xs)
        EQ -> Just <$> UM.read xs pivotIx
        GT -> do
          let lo = pivotIx + 1
          selectSmallST (n - lo) (UM.slice lo (UM.length xs - lo) xs)

-- | Select the n-th smallest element
selectSmall :: (U.Unbox a, Ord a, Show a) => Int -> [a] -> Maybe a
selectSmall n xs =
  runST $ do
    mut <- U.thaw (U.fromList xs)
    selectSmallST n mut

-- | Select the n-th smallest element
modelSelectSmall :: Ord a => Int -> [a] -> Maybe a
modelSelectSmall n xs = List.sort xs `atMay` n

quicksortST :: (Show a, Ord a, U.Unbox a) => UM.MVector s a -> ST s ()
quicksortST xs = do
  mbPivotIx <- partition xs
  case mbPivotIx of
    Nothing -> return ()
    Just pivotIx -> do
      quicksortST (UM.slice 0 pivotIx xs)
      quicksortST (UM.slice (pivotIx + 1) (UM.length xs - (pivotIx + 1)) xs)

quicksort :: (U.Unbox a, Ord a, Show a) => [a] -> [a]
quicksort xs =
  runST $ do
    mut <- U.thaw (U.fromList xs)
    quicksortST mut
    U.toList <$> U.freeze mut
