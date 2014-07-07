---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Composition.Generators.Noise.Turbulence where

import qualified Data.Array.Accelerate     as A
import qualified Math.Coordinate.Cartesian as Cartesian

import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Prelude



turbulence :: (A.Exp Double -> Generator Double) ->
              (A.Exp Double -> Generator Double) ->
              (A.Exp Double -> Generator Double) ->
              (A.Exp Double -> Generator Double) ->
              A.Exp Double ->
              A.Exp Double ->
              Generator Double
turbulence xFun yFun zFun sourceFun power z = Generator $ \point grid ->
    let x0 = Cartesian.x point + (12414.0 / 65536.0)
        y0 = Cartesian.y point + (65124.0 / 65536.0)
        z0 = z                 + (31337.0 / 65536.0)

        x1 = Cartesian.x point + (26519.0 / 65536.0)
        y1 = Cartesian.y point + (18128.0 / 65536.0)
        z1 = z                 + (60493.0 / 65536.0)

        x2 = Cartesian.x point + (53820.0 / 65536.0)
        y2 = Cartesian.y point + (11213.0 / 65536.0)
        z2 = z                 + (44845.0 / 65536.0)

        xDistort = Cartesian.x point + runGenerator (xFun z0) (Cartesian.Point2 x0 y0) grid * power
        yDistort = Cartesian.y point + runGenerator (yFun z1) (Cartesian.Point2 x1 y1) grid * power
        zDistort = z                 + runGenerator (zFun z2) (Cartesian.Point2 x2 y2) grid * power
    in  runGenerator (sourceFun zDistort) (Cartesian.Point2 xDistort yDistort) grid
