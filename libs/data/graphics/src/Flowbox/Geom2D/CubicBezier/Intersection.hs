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



bezierVerticalLineIntersections :: CubicBezier -> Double -> Double -> [Double]
bezierVerticalLineIntersections curve x = Intersection.bezierLineIntersections curve (Line (Point x (-1)) (Point x 1))

valueAtX :: Int -> Double -> CubicBezier -> Double -> Double
valueAtX limit eps curve x = solvey $
    if x <= x1 || err x1 <= eps
        then 0
        else if x >= x4 || err x4 <= eps
            then 1
            else mid $ find 0 startAt
    where CubicBezier (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4) = curve
          startAt    = (0, 1)
          solvex t   = pointX $ eval t
          solvey t   = pointY $ eval t
          eval       = evalBezier curve
          err x'     = abs $ x - x'
          mid (a, b) = (a + b) / 2
          find s t@(a, b)
              | s > limit || err x' <= eps = t
              | otherwise = find (s+1) (if x < x' then (a, m) else (m, b))
              where m  = mid t
                    x' = solvex m
