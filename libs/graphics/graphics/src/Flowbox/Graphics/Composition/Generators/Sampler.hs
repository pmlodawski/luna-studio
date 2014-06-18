---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}

module Flowbox.Graphics.Composition.Generators.Sampler where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate                    as A
import           Math.Space.Space
import           Math.Coordinate.Cartesian                as Cartesian
import           Math.Coordinate.UV                       as UV


sampler :: Matrix2 Double -> Generator
sampler mat pspace pixel = mat M.! A.index2 (A.round y) (A.round x)
    where Z :. h :. w = A.unlift $ shape mat :: EDIM2
          nspace = Grid (A.fromIntegral w) (A.fromIntegral h)
          Cartesian.Point2 x y = toCartesian nspace $ toUV pspace pixel
