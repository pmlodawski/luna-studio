---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Geom2D.CubicBezier.Intersection where

import           Geom2D
import           Geom2D.CubicBezier.Basic
import qualified Geom2D.CubicBezier.Intersection as Intersection

import Flowbox.Prelude



bezierLineIntersections :: CubicBezier -> Double -> Double -> [Double]
bezierLineIntersections curve x = Intersection.bezierLineIntersections curve (Line (Point x (-1)) (Point x 1))
