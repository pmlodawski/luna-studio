---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Geom2D.QuadraticBezier.Basic where

import Data.List
import Geom2D
import Geom2D.CubicBezier.Basic
--import Geom2D.CubicBezier.Intersection
--import Geom2D.CubicBezier.Curvature

import Flowbox.Geom2D.CubicBezier.Basic
import Flowbox.Prelude hiding ((++))

data QuadraticBezier = QuadraticBezier { quadraticC0 :: Point
                                       , quadraticC1 :: Point
                                       , quadraticC2 :: Point
                                       } deriving (Show)

approximateCubicWithQuadratic :: Int -> Double -> CubicBezier -> [QuadraticBezier]
approximateCubicWithQuadratic = approximateCubicWithQuadraticStep 0
    where approximateCubicWithQuadraticStep :: Int -> Int -> Double -> CubicBezier-> [QuadraticBezier]
          approximateCubicWithQuadraticStep step limit eps curve@(CubicBezier pA pB pC pD) =
              if error < eps || step > limit
                  then [approxCruve]
                  else if hasInflections
                      then getSubresults subcurvesInf
                      else if hasExtrema
                          then getSubresults subcurvesExt
                          else getSubresults subcurvesHalf
              where
                  hasInflections = not $ null inflections
                  hasExtrema     = not $ null extremas
                  getSubresults subcurves = foldr1 (++) $ fmap (approximateCubicWithQuadraticStep (step+1) limit eps) subcurves
                  subcurvesExt  = splitBezierN curve extremas
                  subcurvesInf  = splitBezierN curve inflections
                  subcurvesHalf = splitBezierN curve [0.5]
                  extremas     = sort $ findDerivRoots curve 0 1 0.001
                  --extremas     = sort $ curvatureExtrema curve 0.001
                  inflections  = sort $ findBezierInflection curve
                  --
                  approxCruve = QuadraticBezier pA ((c0 ^+^ c1) ^/ 2) pD
                  (QuadraticBezier _ c0 _) = approx pA pB pC
                  (QuadraticBezier _ c1 _) = approx pD pC pB
                  approx a b c      = QuadraticBezier a (approxControl a b) (approxEnd a b c)
                  approxControl a b = (3 *^ b ^-^ a) ^/ 2
                  approxEnd a b c   = a ^-^ 3 *^ b ^+^ 3 *^ c
                  error = ((sqrt 3) / 18) * d
                  d     = vectorDistance c0 c1
