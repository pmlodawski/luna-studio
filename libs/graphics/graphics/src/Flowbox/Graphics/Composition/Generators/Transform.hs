---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Transform where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate                    as A
import           Math.Space.Space
import           Math.Coordinate.Cartesian                (Point2(..))

import           Linear.V2

rotate :: Point2 (Exp Double) -> Exp Double -> Generator -> Generator
rotate (Point2 x0 y0) phi generator (Point2 x1 y1) space = generator (Point2 x2 y2) space
    where x2 = cos phi * dx - sin phi * dy + x0
          y2 = sin phi * dx + cos phi * dy + y0
          dx = x1 - x0
          dy = y1 - y0

translate :: V2 (Exp Double) -> Generator -> Generator
translate (V2 dx dy) generator (Point2 x1 y1) space = generator (Point2 (x1 - dx) (y1 - dy)) space

-- TODO: Rozbić na translate + scale + disort
lensDisort :: Point2 (Exp Double) -> Exp Double -> Exp Double -> Generator -> Generator
lensDisort (Point2 centerX centerY) strength zoom generator (Point2 x y) space@(Grid w h) = generator (Point2 x' y') space
    where newX = x - centerX
          newY = y - centerY
          r = strength * sqrt (newX * newX + newY * newY) / sqrt (w * w + h * h)
          theta = A.cond (abs r A.<=* 1e-6) 1.0 (atan r / r)
          x' = centerX + theta * newX * zoom
          y' = centerY + theta * newY * zoom

-- scale

-- shear
