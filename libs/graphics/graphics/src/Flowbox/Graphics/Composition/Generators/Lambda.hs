---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Lambda where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate as A
import           Math.Space.Space
import           Math.Coordinate.Cartesian (Point2(..))



-- lambdaGenerator a. k. a rasterizer
lambdaGenerator :: Grid (Exp Int) -> Generator -> Matrix2 Double
lambdaGenerator space (Generator gen) = generate (A.index2 h w) wrapper
    where h = height space
          w = width space
          dspace = Grid (A.fromIntegral w) (A.fromIntegral h)
          wrapper (A.unlift -> Z :. y :. x :: EDIM2) = gen pixel dspace
              where pixel = Point2 (A.fromIntegral x) (A.fromIntegral y)
