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

rotate :: Exp Double -> Generator -> Generator
rotate phi generator (Point2 x y) = generator (Point2 x' y')
    where x' = cos phi * x - sin phi * y
          y' = sin phi * x + cos phi * y

translate :: V2 (Exp Double) -> Generator -> Generator
translate (V2 dx dy) generator (Point2 x y) = generator $ Point2 (x - dx) (y - dy)

zoom :: V2 (Exp Double) -> Generator -> Generator
zoom (V2 sx sy) generator (Point2 x y) = generator $ Point2 (x / sx) (y / sy)

-- TODO: Remove and rewrite everything to generator API
disort :: Point2 (Exp Double) -> Generator -> Generator -> Generator
disort (Point2 ox oy) shape generator pixel@(Point2 x y) space@(Grid w h) = generator npixel space
    where r     = shape pixel space
          npixel = Point2 ((x - ox) / r + ox) ((y - oy) / r + oy)

-- scale

-- shear
