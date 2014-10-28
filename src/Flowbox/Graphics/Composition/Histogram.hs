---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Graphics.Composition.Histogram where

import qualified Data.Array.Accelerate as A

import           Flowbox.Prelude
import qualified Flowbox.Math.Matrix    as M
import           Flowbox.Math.Numeric
import qualified Flowbox.Graphics.Utils as U



type Histogram a = A.Acc (A.Vector Int, A.Scalar a, A.Scalar a)
type Histogram' a = (A.Acc (A.Vector Int), A.Acc (A.Scalar a), A.Acc (A.Scalar a))

simpleHistogram :: (A.Elt a, A.IsFloating a, A.Shape sh)
                => A.Exp Int -> A.Acc (A.Array sh a) -> Histogram a
simpleHistogram bins array = histogram lo hi bins array
    where lo = A.the $ A.minimum array
          hi = A.the $ A.maximum array

histogram :: (A.Elt a, A.IsFloating a, A.Shape sh)
          => A.Exp a -> A.Exp a -> A.Exp Int -> A.Acc (A.Array sh a) -> Histogram a
histogram lo hi bins array = A.lift (A.permute (+) zeros hist ones, A.unit lo, A.unit hi)
    where step = getStep lo hi bins

          zeros = A.fill (A.index1 bins) (A.constant 0 :: A.Exp Int)
          ones  = A.fill (A.shape array) (A.constant 1 :: A.Exp Int)

          hist ix = let currentValue = array A.! ix
                    in  A.caseof currentValue [
                          ((A.<=* lo), A.index1 0)
                        , ((A.>=* hi), A.index1 (bins - 1))
                        ]
                        (A.index1 (A.floor $ ((array A.! ix) - lo) / step :: A.Exp Int))

-- histogramK :: (A.Elt a, A.IsFloating a) => A.Exp a -> A.Exp a -> A.Exp Int -> A.Acc (A.Vector a) -> A.Acc (A.Vector Int)
-- histogramK minimal maximal binCount vector = A.generate (A.index1 binCount) $ \(A.unindex1 -> i) ->
--         let calcBin acc value = acc + A.cond (inBin value) 1 0 + A.cond (inEdge value) 1 0

--             valsInBin = (maximal - minimal) / A.fromIntegral binCount
--             iI = A.fromIntegral i
--             iJ = A.fromIntegral $ i + 1
--             inEdge value =     (i A.==* 0            A.&&* value A.<=* minimal)
--                          A.||* (i A.==* binCount - 1 A.&&* value A.>=* maximal)
--             inBin value = (value A.>=* iI * valsInBin A.&&* value A.<* iJ * valsInBin)

--         in A.sfoldl calcBin 0 A.index0 vector

histogramToCDF :: (A.Elt e, A.IsFloating e) => A.Acc (A.Vector Int) -> A.Acc (A.Vector e)
histogramToCDF hist = normalize . cumsum $ hist
    where normalize = A.map (\x -> A.fromIntegral x / sum')
          sum' = A.fromIntegral $ A.the $ A.sum hist

-- | Histogram equalization
histeq :: forall sh e. (A.Shape sh, A.Elt e, A.IsFloating e)
       => A.Exp Int -> A.Acc (A.Array sh e) -> A.Acc (A.Array sh e)
histeq bins arr = lut lo step cdf arr
    where (hist, lo, hi) = A.unlift $ simpleHistogram bins arr :: Histogram' e
          cdf = histogramToCDF hist
          step = getStep (A.the lo) (A.the hi) $ A.length hist

-- | Equalize histogram of array to the given one
histeq' :: forall sh e. (A.Shape sh, A.Elt e, A.IsFloating e)
        => Histogram e -> A.Acc (A.Array sh e) -> A.Acc (A.Array sh e)
histeq' (A.unlift -> (hist, lo, hi) :: Histogram' e) = lut lo step cdf
    where cdf = histogramToCDF hist
          step = getStep (A.the lo) (A.the hi) $ A.length hist

getStep :: (A.Elt e, A.IsFloating e) => A.Exp e -> A.Exp e -> A.Exp Int -> A.Exp e
getStep lo hi bins = (hi - lo) / (A.fromIntegral bins - 1)

lut :: (A.Shape sh, A.Elt e, A.IsFloating e)
    => A.Acc (A.Scalar e)
    -> A.Exp e
    -> A.Acc (A.Vector e)
    -> A.Acc (A.Array sh e)
    -> A.Acc (A.Array sh e)
lut (A.the -> lo) step cdf matrix = A.map (\x -> cdf A.!! A.floor ((x - lo) / step)) matrix
