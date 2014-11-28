---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}

module Flowbox.Geom2D.QuadraticBezier.Conversion where

import           Data.List
import           Geom2D
import qualified Geom2D.CubicBezier.Basic as Cubic

import Flowbox.Geom2D.Conversion
import Flowbox.Geom2D.CubicBezier
import Flowbox.Geom2D.CubicBezier.Conversion
import Flowbox.Geom2D.QuadraticBezier
import Flowbox.Geom2D.CubicBezier.Solve
import Flowbox.Prelude                       hiding ((++))



approximateCubicWithQuadratic :: Int -> Double -> CubicBezier Double -> [QuadraticBezier Double]
approximateCubicWithQuadratic = approximateCubicWithQuadraticStep 1
    where approximateCubicWithQuadraticStep :: Int -> Int -> Double -> CubicBezier Double-> [QuadraticBezier Double]
          approximateCubicWithQuadraticStep step limit eps curve@(CubicBezier pA pB pC pD)
              | err < eps || step >= limit = [approxCruve]
              | hasInflections             = getSubresults subcurvesInf
              | hasExtrema                 = getSubresults subcurvesExt
              | otherwise                  = getSubresults subcurvesHalf
              where
                  curve' = fcb2gcb curve
                  hasInflections = not $ null inflections
                  hasExtrema     = not $ null extremas
                  getSubresults subcurves = foldr1 (++) $ fmap (approximateCubicWithQuadraticStep (step+1) limit eps) subcurves
                  splitN w      = fmap gcb2fcb $ Cubic.splitBezierN curve' w
                  subcurvesExt  = splitN extremas
                  subcurvesInf  = splitN inflections
                  subcurvesHalf = splitN [0.5]
                  extremas     = sort $ filter (\x -> x > 0 && x < 1) $ findDerivRoots curve 0 1 0.001 -- INFO: 0 and 1 roots cause the splitBezierN to return NaNs
                  --extremas     = sort $ curvatureExtrema curve 0.001
                  inflections  = sort $ Cubic.findBezierInflection curve'
                  --
                  approxCruve = QuadraticBezier pA ((c0 + c1) / 2) pD
                  (QuadraticBezier _ c0 _) = approx pA pB pC
                  (QuadraticBezier _ c1 _) = approx pD pC pB
                  --approx :: Point2 a -> Point2 a -> Point2 a -> QuadraticBezier a
                  approx a b c      = QuadraticBezier a (approxControl a b) (approxEnd a b c)
                  --approxControl :: Point2 a -> Point2 a -> Point2 a
                  approxControl a b = (3 * b - a) / 2
                  --approxEnd :: Point2 a -> Point2 a -> Point2 a -> Point2 a
                  approxEnd a b c   = a - 3 * b + 3 * c
                  err = (sqrt 3 / 18) * d
                  d   = vectorDistance (fp2gp c0) (fp2gp c1)

convertCubicsToQuadratics :: Int -> Double -> [CubicBezier Double] -> [QuadraticBezier Double]
convertCubicsToQuadratics steps eps cubics = concat $ fmap (approximateCubicWithQuadratic steps eps) cubics
