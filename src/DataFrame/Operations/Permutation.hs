{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module DataFrame.Operations.Permutation where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Control.Exception (throw)
import Control.Monad.ST (runST)
import Data.Vector.Internal.Check (HasCallStack)
import DataFrame.Errors (DataFrameException (..))
import DataFrame.Internal.Column (Columnable, atIndicesStable)
import DataFrame.Internal.DataFrame (DataFrame (..))
import DataFrame.Internal.Expression (Expr (Col))
import DataFrame.Internal.Row (sortedIndexes', toRowVector)
import DataFrame.Operations.Core (columnNames, dimensions)
import System.Random (Random (randomR), RandomGen)

-- | Sort order taken as a parameter by the 'sortBy' function.
data SortOrder where
    Asc :: (Columnable a) => Expr a -> SortOrder
    Desc :: (Columnable a) => Expr a -> SortOrder

instance Eq SortOrder where
    (==) :: SortOrder -> SortOrder -> Bool
    (==) (Asc _) (Asc _) = True
    (==) (Desc _) (Desc _) = True
    (==) _ _ = False

getSortColumnName :: SortOrder -> T.Text
getSortColumnName (Asc (Col n)) = n
getSortColumnName (Desc (Col n)) = n
getSortColumnName _ = error "Sorting on compound column"

mustFlipCompare :: SortOrder -> Bool
mustFlipCompare (Asc _) = True
mustFlipCompare (Desc _) = False

{- | O(k log n) Sorts the dataframe by a given row.

> sortBy Ascending ["Age"] df
-}
sortBy ::
    [SortOrder] ->
    DataFrame ->
    DataFrame
sortBy sortOrds df
    | any (`notElem` columnNames df) names =
        throw $
            ColumnsNotFoundException
                (names L.\\ columnNames df)
                "sortBy"
                (columnNames df)
    | otherwise =
        let
            indexes = sortedIndexes' mustFlips (toRowVector names df)
         in
            df{columns = V.map (atIndicesStable indexes) (columns df)}
  where
    names = map getSortColumnName sortOrds
    mustFlips = map mustFlipCompare sortOrds

shuffle ::
    (RandomGen g) =>
    g ->
    DataFrame ->
    DataFrame
shuffle pureGen df =
    let
        indexes = shuffledIndices pureGen (fst (dimensions df))
     in
        df{columns = V.map (atIndicesStable indexes) (columns df)}

shuffledIndices :: (HasCallStack, RandomGen g) => g -> Int -> VU.Vector Int
shuffledIndices pureGen k
    | k < 0 = error $ "Vector index may not be a neative number: " <> show k
    | k == 0 = VU.empty
    | otherwise = shuffleVec pureGen
  where
    shuffleVec :: (RandomGen g) => g -> VU.Vector Int
    shuffleVec g = runST $ do
        vm <- VUM.generate k id
        let (n, nGen) = randomR (1, k - 1) g
        go vm n nGen
        VU.unsafeFreeze vm

    go v (-1) _ = pure ()
    go v 0 _ = pure ()
    go v maxInd gen =
        let
            (n, nextGen) = randomR (1, maxInd) gen
         in
            VUM.swap v 0 n *> go (VUM.tail v) (maxInd - 1) nextGen
