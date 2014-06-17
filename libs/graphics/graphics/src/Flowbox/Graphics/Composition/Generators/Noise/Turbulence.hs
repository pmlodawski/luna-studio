---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Composition.Generators.Noise.Turbulence where

import qualified Data.Array.Accelerate as A

import Flowbox.Prelude



turbulence :: (A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float) ->
              (A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float) ->
              (A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float) ->
              (A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float) ->
              A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float
turbulence xFun yFun zFun sourceFun power x y z = sourceFun xDistort yDistort zDistort
    where x0 = x + (12414.0 / 65536.0)
          y0 = y + (65124.0 / 65536.0)
          z0 = z + (31337.0 / 65536.0)

          x1 = x + (26519.0 / 65536.0)
          y1 = y + (18128.0 / 65536.0)
          z1 = z + (60493.0 / 65536.0)

          x2 = x + (53820.0 / 65536.0)
          y2 = y + (11213.0 / 65536.0)
          z2 = z + (44845.0 / 65536.0)

          xDistort = x + (xFun x0 y0 z0 * power)
          yDistort = y + (yFun x1 y1 z1 * power)
          zDistort = z + (zFun x2 y2 z2 * power)
