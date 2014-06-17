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



type GradientVector = V2 (Cartesian.Point2 Double)
colorMapper :: GradientVector -> [Tick Double]
            -> (Exp Double -> Exp Double -> Exp Double -> Exp Double)
            -> (Exp GradientVector -> Generator)
            -> Generator
colorMapper vector ticks ftrans shapeGenerator pspace pixel = sfoldl findColor (0.0 :: Exp Double) index0 zippedTicks
    where zippedTicks = A.zip accticks $ A.tail accticks
          accticks    = A.use $ fromList (Z :. length ticksNorm) ticksNorm
          ticksNorm   = firstElem : sort ticks P.++ [lastElem]
          firstElem   = head ticks & position .~ -1e20
          lastElem    = last ticks & position .~ 1e20
          vec         = variable vector

          grad_pos = shapeGenerator vec pspace pixel

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


radialShape :: (MetricCoord a Cartesian, Metric a (Cartesian.Point2 (Exp Double)) (Exp Double)) 
            => a -> Exp GradientVector -> Generator
radialShape metric vector space pixel = distance ms ubegin upixel / distance ms ubegin uend
    where ms = MetricSpace metric space
          vec = over each unlift $ unlift vector :: V2 (Cartesian.Point2 (Exp Double))
          V2 ubegin uend = over each (toUV space) vec
          upixel = toUV space pixel

circularShape :: Exp GradientVector -> Generator
circularShape = radialShape Euclidean

diamondShape :: Exp GradientVector -> Generator
diamondShape = radialShape Taxicab

squareShape :: Exp GradientVector -> Generator
squareShape  = radialShape Chebyshev

conicalShape :: Exp GradientVector -> Generator
conicalShape vector space pixel = min (res A.>* 1.0 ? (res - 1.0, res)) 1.0
    where V2 begin end = over each unlift $ unlift vector :: V2 (Cartesian.Point2 (Exp Double))
          a1 = Cartesian.uncurry atan2 $ end - begin
          a2 = Cartesian.uncurry atan2 $ begin - pixel
          res = 1.0 - (a2 + a1) / (2.0 * pi)

linearShape :: Exp GradientVector -> Generator
linearShape vector space pixel = dsum * (1.0 / dmod)
    where V2 begin end = over each unlift $ unlift vector :: V2 (Cartesian.Point2 (Exp Double))
          deltav = end - begin
          deltap = pixel - begin
          dsum = Cartesian.uncurry (+) $ deltap * deltav -- dot product of free vectors TODO: integrate with linear library
          dmod = Cartesian.uncurry (+) $ deltav * deltav
