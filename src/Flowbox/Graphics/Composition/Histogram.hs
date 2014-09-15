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
simpleHistogram bins array = histogram mini maxi bins array
    where mini = A.the $ A.minimum array
          maxi = A.the $ A.maximum array

histogram :: (A.Elt a, A.IsFloating a, A.Shape sh)
          => A.Exp a -> A.Exp a -> A.Exp Int -> A.Acc (A.Array sh a) -> Histogram a
histogram mini' maxi' bins' array = A.lift (A.permute (+) zeros hist ones, A.unit mini, A.unit maxi)
    where mini = U.variable mini'
          maxi = U.variable maxi'
          bins = U.variable bins'
          step = (maxi - mini) / (A.fromIntegral bins - 1)

          zeros = A.fill (A.index1 bins) (A.constant 0 :: A.Exp Int)
          ones  = A.fill (A.shape array) (A.constant 1 :: A.Exp Int)

          hist ix = A.index1 (A.ceiling $ ((array A.! ix) - mini) / step :: A.Exp Int)

histogramToCDF :: (A.Elt e, A.IsFloating e) => A.Acc (A.Vector Int) -> A.Acc (A.Vector e)
histogramToCDF hist = normalize . cumsum $ hist
    where normalize = A.map (\x -> A.fromIntegral x / sum')
          sum' = A.fromIntegral $ A.the $ A.sum hist

-- | Histogram equalization
histeq :: forall sh e. (A.Shape sh, A.Elt e, A.IsFloating e)
       => A.Exp Int -> A.Acc (A.Array sh e) -> A.Acc (A.Array sh e)
histeq bins arr = lut mini cdf arr
    where (hist, mini, maxi) = A.unlift $ simpleHistogram bins arr :: Histogram' e
          cdf = histogramToCDF hist

-- | Equalize histogram of array to the given one
histeq' :: forall sh e. (A.Shape sh, A.Elt e, A.IsFloating e)
        => Histogram e -> A.Acc (A.Array sh e) -> A.Acc (A.Array sh e)
histeq' (A.unlift -> (hist, mini, maxi) :: Histogram' e) = lut mini cdf
    where cdf = histogramToCDF hist

lut :: (A.Shape sh, A.Elt e, A.IsFloating e)
    => A.Acc (A.Scalar e) -> A.Acc (A.Vector e)
    -> A.Acc (A.Array sh e)
    -> A.Acc (A.Array sh e)
lut (A.the -> mini) cdf = A.map (\x -> cdf A.!! A.ceiling ((x - mini) / step))
    where step = (cdf A.!! 1) - (cdf A.!! 0)
