---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}

module Flowbox.Graphics.Composition.Generators.Kernel where

import Flowbox.Prelude                                    as P
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate as A
import           Math.Space.Space
import           Math.Coordinate.UV

bilinear :: Generator
bilinear _ _ = 1.0

gaussian :: Generator
gaussian pixel space = exp $ (-(x ** 2 + y ** 2) * (exp pi))
    where Point2 x y = toUV space pixel - Point2 0.5 0.5
