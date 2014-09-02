---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Geom2D.CubicBezier.Basic where

import Math.BernsteinPoly
import Geom2D.CubicBezier.Basic
import Geom2D.CubicBezier.Intersection

import Flowbox.Prelude



findDerivRoots :: CubicBezier -> Double -> Double -> Double -> [Double]
findDerivRoots curve boundLo boundHi eps = uncurry (++) roots
    where roots    = over each bfr bern'
          bfr poly = bezierFindRoot poly boundLo boundHi eps
          bern' = over each bernsteinDeriv bern
          bern = bezierToBernstein curve
