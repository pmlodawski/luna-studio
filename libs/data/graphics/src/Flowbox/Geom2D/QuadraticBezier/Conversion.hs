---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module Flowbox.Geom2D.QuadraticBezier.Conversion where

import           Data.List
import           Debug.Trace
import           Geom2D
import qualified Geom2D.CubicBezier.Basic as Cubic

import Flowbox.Geom2D.Conversion
import Flowbox.Geom2D.CubicBezier
import Flowbox.Geom2D.CubicBezier.Conversion
import Flowbox.Geom2D.QuadraticBezier
import Flowbox.Geom2D.CubicBezier.Solve
import Flowbox.Prelude                       hiding ((++))



approximateCubicWithQuadratic :: Int -> Float -> CubicBezier Float -> [QuadraticBezier Float]
approximateCubicWithQuadratic = approximateCubicWithQuadraticStep 1
    where approximateCubicWithQuadraticStep :: Int -> Int -> Float -> CubicBezier Float-> [QuadraticBezier Float]
          approximateCubicWithQuadraticStep step limit eps curve@(CubicBezier pA pB pC pD)
              | err < eps || step >= limit = [approxCurve]
              | hasInflections             = getSubresults subcurvesInf
              | hasExtrema                 = getSubresults subcurvesExt
              | otherwise                  = getSubresults subcurvesHalf
              where
                  curve' = fcb2gcb curve
                  hasInflections = not $ null inflections
                  hasExtrema     = not $ null extremas
                  getSubresults subcurves = foldr1 (++) $ fmap (approximateCubicWithQuadraticStep (step+1) limit eps) subcurves
                  splitN (fmap realToFrac -> w) = fmap gcb2fcb $ Cubic.splitBezierN curve' w
                  subcurvesExt  = splitN extremas
                  subcurvesInf  = splitN inflections
                  subcurvesHalf = splitN [0.5]
                  extremas     = sort $ filter (\x -> x > 0 && x < 1) $ findDerivRoots curve 0 1 0.001 -- INFO: 0 and 1 roots cause the splitBezierN to return NaNs
                  --extremas     = sort $ curvatureExtrema curve 0.001
                  inflections  = sort $ Cubic.findBezierInflection curve'
                  --
                  approxCurve = QuadraticBezier pA ((c0 + c1) / 2) pD
                  (QuadraticBezier _ c0 _) = approx pA pB pC
                  (QuadraticBezier _ c1 _) = approx pD pC pB
                  --approx :: Point2 a -> Point2 a -> Point2 a -> QuadraticBezier a
                  approx a b c      = QuadraticBezier a (approxControl a b) (approxEnd a b c)
                  --approxControl :: Point2 a -> Point2 a -> Point2 a
                  approxControl a b = (3 * b - a) / 2
                  --approxEnd :: Point2 a -> Point2 a -> Point2 a -> Point2 a
                  approxEnd a b c   = a - 3 * b + 3 * c
                  err = (sqrt 3 / 18) * d
                  d   = realToFrac $ vectorDistance (fp2gp c0) (fp2gp c1)

approximateCubicWithQuadratic' :: Int -> Float -> CubicBezier Float -> [QuadraticBezier Float]
approximateCubicWithQuadratic' = approximateCubicWithQuadraticStep 1
    where approximateCubicWithQuadraticStep :: Int -> Int -> Float -> CubicBezier Float -> [QuadraticBezier Float]
          approximateCubicWithQuadraticStep step limit eps curve@(CubicBezier pA pB pC pD)
              | err < eps || step >= limit = [approxCurve]
              | otherwise                  = getSubresults subcurvesHalf
              where
                  (QuadraticBezier _ c0 _) = approx pA pB pC -- ok
                  (QuadraticBezier _ c1 _) = approx pD pC pB -- ok
                  approxCurve = QuadraticBezier pA ((3 * c1 - pD + 3 * c0 - pA) / 4) pD -- ok
                  d   = realToFrac $ vectorDistance (fp2gp c0) (fp2gp c1) -- distance between control points
                  err = (sqrt 3 / 18) * d -- this should be precision, but something is fucky (no t^3_max)
                  approx a b c = QuadraticBezier a (approxControl a b) (approxEnd a b c) -- ok
                  approxControl a b = (3 * b - a) / 2   -- ok
                  approxEnd a b c   = a - 3 * b + 3 * c -- ok
                  inflections  = sort $ filter (\x -> x > 0 && x < 1) $ Cubic.findBezierInflection curve'
                  hasInflections = {-trace ("running hasInflections with inflections =" ++ show inflections) $-} not $ null inflections
                  subcurvesInf  = splitN inflections
                  subcurvesHalf = splitN [0.5] -- not cool, half doesn't seem to be useful here.
                  getSubresults subcurves = foldr1 (++) $ fmap (approximateCubicWithQuadraticStep (step+1) limit eps) subcurves -- ok
                  curve' = fcb2gcb curve
                  splitN w      = fmap gcb2fcb $ Cubic.splitBezierN curve' w -- seems ok





convertCubicsToQuadratics :: Int -> Float -> [CubicBezier Float] -> [QuadraticBezier Float]
convertCubicsToQuadratics steps eps cubics = concat $ fmap (approximateCubicWithQuadratic' steps eps) cubics
