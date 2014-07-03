---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Composition.Generators.Gradient where

import Flowbox.Prelude                                    as P hiding ((?))
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Utils

import Data.Array.Accelerate                              as A
import Data.List                                          (sort)
import Math.Coordinate
import Math.Coordinate.Cartesian                          as Cartesian hiding (x, y, w)
import Math.Metric                                        hiding (metric, space)
import Math.Space.Space



colorMapper :: [Tick Double]
            -> (Exp Double -> Exp Double -> Exp Double -> Exp Double)
            -> Generator -> Generator
colorMapper ticks ftrans shapeGenerator = Generator $ \pixel pspace -> 
    let zippedTicks = A.zip accticks $ A.tail accticks
        accticks    = A.use $ fromList (Z :. P.length ticksNorm) ticksNorm
        ticksNorm   = firstElem : sort ticks P.++ [lastElem]
        firstElem   = head ticks & position .~ -1e20
        lastElem    = last ticks & position .~ 1e20

        grad_pos = runGenerator shapeGenerator pixel pspace

        findColor acc positions = (grad_pos >=* aPos &&* grad_pos A.<* nPos) ? (newColor, acc)
            where (actualPos, nextPos) = unlift positions :: (Exp (Tick Double), Exp (Tick Double))
                  aPos = unlift actualPos ^. position 
                  aVal = unlift actualPos ^. value
                  aWei = unlift actualPos ^. weight

                  nPos = unlift nextPos ^. position
                  nVal = unlift nextPos ^. value
                  nWei = unlift nextPos ^. weight

                  prop = ftrans aWei nWei $ (grad_pos - aPos) / (nPos - aPos)
                  newColor = mix prop aVal nVal
    in sfoldl findColor (0.0 :: Exp Double) index0 zippedTicks

radialShape :: (MetricCoord a Cartesian, Metric a (Point2 (Exp Double)) (Exp Double)) => a -> Generator
radialShape metric = Generator $ \pixel space -> let ms = MetricSpace metric space
                                                 in distance ms (Point2 0 0) pixel

circularShape :: Generator
circularShape = radialShape Euclidean

diamondShape :: Generator
diamondShape = radialShape Taxicab

squareShape :: Generator
squareShape  = radialShape Chebyshev

conicalShape :: Generator
conicalShape = Generator $ \pixel _ -> let res = 1.0 - Cartesian.uncurry atan2 pixel / (2.0 * pi)
                                        in min (res A.>* 1.0 ? (res - 1.0, res)) 1.0

linearShape :: Generator
linearShape = Generator $ \(Point2 x _) (Grid w _) -> x / w
