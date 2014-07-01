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
import Linear.Accelerate                                  ()
import Linear.V2
import Math.Coordinate
import Math.Coordinate.Cartesian                          as Cartesian
import Math.Coordinate.UV                                 as UV
import Math.Metric
import Math.Space.Space



colorMapper :: [Tick Double]
            -> (Exp Double -> Exp Double -> Exp Double -> Exp Double)
            -> Generator -> Generator
colorMapper ticks ftrans shapeGenerator pixel pspace = sfoldl findColor (0.0 :: Exp Double) index0 zippedTicks
    where zippedTicks = A.zip accticks $ A.tail accticks
          accticks    = A.use $ fromList (Z :. length ticksNorm) ticksNorm
          ticksNorm   = firstElem : sort ticks P.++ [lastElem]
          firstElem   = head ticks & position .~ -1e20
          lastElem    = last ticks & position .~ 1e20

          grad_pos = shapeGenerator pixel pspace

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

radialShape :: (MetricCoord a Cartesian, Metric a (Cartesian.Point2 (Exp Double)) (Exp Double)) => a -> Generator
radialShape metric pixel space@(Grid w h) = distance ms (Cartesian.Point2 0 0) pixel
    where ms = MetricSpace metric space

circularShape :: Generator
circularShape = radialShape Euclidean

diamondShape :: Generator
diamondShape = radialShape Taxicab

squareShape :: Generator
squareShape  = radialShape Chebyshev

conicalShape :: Generator
conicalShape pixel space = min (res A.>* 1.0 ? (res - 1.0, res)) 1.0
    where res = 1.0 - Cartesian.uncurry atan2 pixel / (2.0 * pi)

linearShape :: Generator
linearShape (Cartesian.Point2 x _) (Grid w _) = x / w
