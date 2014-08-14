---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Gradient where

import Flowbox.Prelude                                    as P hiding ((?))
import Flowbox.Graphics.Composition.Generators.Structures

import Data.Array.Accelerate                              as A
import Data.List                                          (sort)
import Math.Coordinate
import Math.Coordinate.Cartesian                          as Cartesian hiding (x, y, w)
import Math.Metric


colorMapper :: forall a b c x . (Elt a, Elt b, Elt c, IsFloating a, Num a, Ord a) 
            => [Tick a b c] -> (Exp a -> Exp b -> Exp c -> Exp b -> Exp c -> Exp a) -> Generator x (Exp a) -> Generator x (Exp a)
colorMapper ticks weightFun shapeGenerator = Generator $ \pixel ->
    let zippedTicks = A.zip accticks $ A.tail accticks
        accticks    = A.use $ fromList (Z :. P.length ticksNorm) ticksNorm
        ticksNorm   = firstElem : sort ticks P.++ [lastElem]
        firstElem   = head ticks & position .~ -1e20 -- FIXME [KL]: Do something with this constants
        lastElem    = last ticks & position .~ 1e20

        gradPos = runGenerator shapeGenerator pixel

        findColor acc positions = (gradPos >=* aPos &&* gradPos A.<* nPos) ? (newColor, acc)
            where (actualPos, nextPos) = unlift positions :: (Exp (Tick a b c), Exp (Tick a b c))
                  aPos = unlift actualPos ^. position 
                  aVal = unlift actualPos ^. value
                  aWei = unlift actualPos ^. weight

                  nPos = unlift nextPos ^. position
                  nVal = unlift nextPos ^. value
                  nWei = unlift nextPos ^. weight

                  tickPos = (gradPos - aPos) / (nPos - aPos)
                  newColor = weightFun tickPos aVal aWei nVal nWei

    in sfoldl findColor 0 index0 zippedTicks

radialShape :: (Num b, MetricCoord a Cartesian, Metric a b c) => a -> Generator b c
radialShape metric = Generator $ \pixel -> distanceBase metric 0 pixel

circularShape :: (Num a, Metric Euclidean a b) => Generator a b
circularShape = radialShape Euclidean

diamondShape :: (Num a, Metric Taxicab a b) => Generator a b
diamondShape = radialShape Taxicab

squareShape :: (Num a, Metric Chebyshev a b) => Generator a b
squareShape  = radialShape Chebyshev

conicalShape :: (Elt a, IsFloating a) => CartesianGenerator (Exp a) (Exp a)
conicalShape = Generator $ \pixel -> let res = 1 - Cartesian.uncurry atan2 pixel / (2 * pi)
                                      in min (res A.>* 1 ? (res - 1, res)) 1

linearShape :: Fractional a => CartesianGenerator a a
linearShape = Generator $ \(Point2 x _) -> x
