---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Composition.Generator.Noise.Turbulence where

import qualified Data.Array.Accelerate          as A
import qualified Math.Coordinate.Cartesian      as Cartesian

import           Flowbox.Graphics.Shader.Shader
import           Flowbox.Prelude



--turbulence :: (A.Elt a, A.IsFloating a, a)
--           => (A.Exp a -> ContinuousShader (A.Exp a))
--           -> (A.Exp a -> ContinuousShader (A.Exp a))
--           -> (A.Exp a -> ContinuousShader (A.Exp a))
--           -> (A.Exp a -> ContinuousShader (A.Exp a))
--           -> A.Exp a -> A.Exp a
--           -> ContinuousShader (A.Exp a)
turbulence :: (A.Exp Float -> ContinuousShader (A.Exp Float))
           -> (A.Exp Float -> ContinuousShader (A.Exp Float))
           -> (A.Exp Float -> ContinuousShader (A.Exp Float))
           -> (A.Exp Float -> ContinuousShader (A.Exp Float))
           -> A.Exp Float -> A.Exp Float
           -> ContinuousShader (A.Exp Float)
turbulence xFun yFun zFun sourceFun power z = unitShader $ \point ->
    let x0 = Cartesian.x point + (12414.0 / 65536.0)
        y0 = Cartesian.y point + (65124.0 / 65536.0)
        z0 = z                 + (31337.0 / 65536.0)

        x1 = Cartesian.x point + (26519.0 / 65536.0)
        y1 = Cartesian.y point + (18128.0 / 65536.0)
        z1 = z                 + (60493.0 / 65536.0)

        x2 = Cartesian.x point + (53820.0 / 65536.0)
        y2 = Cartesian.y point + (11213.0 / 65536.0)
        z2 = z                 + (44845.0 / 65536.0)

        xDistort = Cartesian.x point + runShader (xFun z0) (Cartesian.Point2 x0 y0) * power
        yDistort = Cartesian.y point + runShader (yFun z1) (Cartesian.Point2 x1 y1) * power
        zDistort = z                 + runShader (zFun z2) (Cartesian.Point2 x2 y2) * power
    in  runShader (sourceFun zDistort) (Cartesian.Point2 xDistort yDistort)
