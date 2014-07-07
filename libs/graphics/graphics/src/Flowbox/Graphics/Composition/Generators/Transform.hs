---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Transform where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Math.Matrix                                as M

import           Math.Coordinate.Cartesian                (Point2(..))

import           Linear.V2

rotate :: Exp Double -> Generator -> Generator
rotate phi (Generator gen) = Generator $ \(Point2 x y) -> 
    let x' = cos phi * x - sin phi * y
        y' = sin phi * x + cos phi * y
    in gen (Point2 x' y')

translate :: V2 (Exp Double) -> Generator -> Generator
translate (V2 dx dy) (Generator gen) = Generator $ \(Point2 x y) -> gen $ Point2 (x - dx) (y - dy)

scale :: V2 (Exp Double) -> Generator -> Generator
scale (V2 sx sy) (Generator gen) = Generator $ \(Point2 x y) -> gen $ Point2 (x / sx) (y / sy)
