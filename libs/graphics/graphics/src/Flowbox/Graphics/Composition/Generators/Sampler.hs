---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Sampler where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate                    as A
import           Math.Space.Space
import           Math.Coordinate.Cartesian                (Point2(..), toCartesian)
import           Math.Coordinate.UV                       (toUV)


nearest :: Boundary (Exp Double) -> Matrix2 Double -> Generator
nearest b mat (Point2 x y) _ = boundedIndex b mat $ A.index2 (A.truncate y) (A.truncate x)

--nearest :: Boundary (Exp Double) -> Matrix2 Double -> Generator
--nearest = bicubic box

bilinear :: Boundary (Exp Double) -> Matrix2 Double -> Generator
bilinear = bicubic triangle

bicubic :: Filter -> Boundary (Exp Double) -> Matrix2 Double -> Generator
bicubic filter b mat pixel newSpace = A.uncurry (/) result
    where Z :. oldHeight :. oldWidth = A.unlift $ shape mat :: EDIM2
          oldSpace = Grid (A.fromIntegral oldWidth) (A.fromIntegral oldHeight)
          Point2 x' y' = toCartesian oldSpace $ toUV newSpace pixel

          fs = 6 -- Should be: A.floor $ 2 * window filter -- TODO: conditional based on the scale ratio
          dxs = generate (A.index2 fs fs) $ \(A.unlift -> Z :. _ :. x :: EDIM2) -> x - 2
          dys = generate (A.index2 fs fs) $ \(A.unlift -> Z :. y :. _ :: EDIM2) -> y - 2

          start = A.lift (0.0 :: Exp Double, 0.0 :: Exp Double) :: Exp (Double, Double)
          result = sfoldl calc start A.index0 (flatten $ M.zip dxs dys)

          xs  = let xs' = width newSpace / width oldSpace   in A.cond (xs' A.<* 1.0) xs' 1.0
          ys  = let ys' = height newSpace / height oldSpace in A.cond (ys' A.<* 1.0) ys' 1.0

          calc (A.unlift -> (valSum, weightSum)) (A.unlift -> (dx, dy)) = A.lift (valSum + value * weight, weightSum + weight)
              where value  = boundedIndex b mat $ A.index2 (A.floor y' + dy) (A.floor x' + dx)
                    wx = apply filter $ (A.fromIntegral dx - frac x') * xs
                    wy = apply filter $ (A.fromIntegral dy - frac y') * ys
                    weight = wx * wy
