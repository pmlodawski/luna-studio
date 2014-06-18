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
import Math.Coordinate.Cartesian                          (Point2(..))



multisampler :: Matrix2 Double -> Generator -> Generator
multisampler kernel generator point space = A.uncurry (/) result
    where Z :. h :. w = unlift $ shape kernel :: EDIM2
          dxs = generate (shape kernel) $ \i -> let Z :. _ :. x = unlift i :: EDIM2 in A.fromIntegral (x - (w `div` 2))
          dys = generate (shape kernel) $ \i -> let Z :. y :. _ = unlift i :: EDIM2 in A.fromIntegral (y - (h `div` 2))
          off = flatten $ M.zip3 kernel dxs dys
          start = lift (0.0 :: Exp Double, 0.0 :: Exp Double) :: Exp (Double, Double)
          result = sfoldl calc start index0 off

          calc acc p = lift (acc_values + weight * generator (point + offset) space, acc_weights + weight) :: Exp (Double, Double)
              where offset = Point2 (dx / A.fromIntegral w) (dy / A.fromIntegral h)
                    (weight, dx, dy) = unlift p :: (Exp Double, Exp Double, Exp Double)
                    (acc_values, acc_weights) = unlift acc :: (Exp Double, Exp Double)
