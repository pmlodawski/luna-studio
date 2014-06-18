---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}

module Flowbox.Graphics.Composition.Generators.Multisampler where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Math.Matrix                                as M

import Data.Array.Accelerate                              as A (fromIntegral, lift, unlift, index0, uncurry) 
import Math.Coordinate.Cartesian                          as Cartesian
import Math.Space.Space



normalize :: Matrix2 Double -> Matrix2 Double
normalize arr = M.map (/values_sum) arr
    where values_sum = the $ foldAll (+) (0.0 :: Exp Double) (flatten arr)

uniform :: Matrix2 Double -> Matrix2 Double
uniform arr = M.map (/max_value) arr
    where max_value = the $ M.maximum (flatten arr)

-- TODO: Debug
gaussianKernel :: Exp DIM2 -> Exp Double -> Matrix2 Double
gaussianKernel sh' sigma' = uniform $ normalize $ generate sh $ \i -> let Z :. y :. x = unlift i :: (Z :. Exp Int :. Exp Int)
                                                                      in gaussian (A.fromIntegral x - width_half)
                                                                                  (A.fromIntegral y - height_half)
    where sigma = the $ unit sigma'
          sh = the $ unit sh'
          Z :. height :. width = unlift sh :: EDIM2
          height_half = A.fromIntegral $ height `div` 2
          width_half = A.fromIntegral $ width `div` 2
          sg = 2 * sigma ** 2
          e = exp 1.0
          gaussian x y = -(e / (pi * sg) - (x ** 2 + y ** 2) / sg)

multisampler :: Matrix2 Double -> Generator -> Generator
multisampler kernel generator point space = A.uncurry (/) result
    where Z :. h :. w = unlift $ shape kernel :: EDIM2
          dxs = generate (shape kernel) $ \i -> let Z :. y :. x = unlift i :: EDIM2 in A.fromIntegral (x - (w `div` 2))
          dys = generate (shape kernel) $ \i -> let Z :. y :. x = unlift i :: EDIM2 in A.fromIntegral (y - (h `div` 2))
          off = flatten $ M.zip3 kernel dxs dys
          start = lift (0.0 :: Exp Double, 0.0 :: Exp Double) :: Exp (Double, Double)
          result = sfoldl calc start index0 off

          calc acc p = lift (acc_values + weight * generator (point + offset) space, acc_weights + weight) :: Exp (Double, Double)
              where offset = Cartesian.Point2 (dx / A.fromIntegral w) (dy / A.fromIntegral h)
                    (weight, dx, dy) = unlift p :: (Exp Double, Exp Double, Exp Double)
                    (acc_values, acc_weights) = unlift acc :: (Exp Double, Exp Double)
