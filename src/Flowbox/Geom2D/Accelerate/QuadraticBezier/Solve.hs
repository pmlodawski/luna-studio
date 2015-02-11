---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve where

import Data.Array.Accelerate     as A
import Math.Coordinate.Cartesian (Point2 (..))

import           Flowbox.Geom2D.Accelerate.QuadraticBezier ()
import           Flowbox.Geom2D.QuadraticBezier
import           Flowbox.Graphics.Utils.Utils              hiding (sign)
import qualified Flowbox.Graphics.Utils.Accelerate as A
import           Flowbox.Prelude                                hiding ((<*))
import qualified Flowbox.Prelude                                as P hiding ((<*))

import qualified Flowbox.Math.Matrix                            as M



cuberoot :: Exp Float -> Exp Float
cuberoot x = signum x * pow (abs x)
    where pow = flip (**) (1/3)

solveCubic :: Exp Float -> Exp Float -> Exp Float -> Exp (Int, (Float, Float, Float))
solveCubic a b c = A.cond (d >=* 0) opt1 opt3
    where opt1 = A.lift ( 1::Int, (offset + u + v, 0::Float, 0::Float) )
          opt3 = A.lift ( 3::Int, (offset + u' * (m + m), offset - u' * (n + m), offset + u' * (n - m)) )
          d = q*q + 4*p3 / 27
          q = a * (2*a*a - 9*b) / 27 + c
          p3 = p*p*p
          p = b - a*a / 3
          offset = (-a) / 3
          u = cuberoot $ ((-q) + z) / 2
          v = cuberoot $ ((-q) - z) / 2
          z = sqrt d
          u' = sqrt $ (-p) / 3
          m = w
          n = sin v' * 1.732050808 --TODO rozwin w szereg fouriera
          v' = acos w --TODO rozwin w szereg fouriera
          w = (-sqrt ((-27) / p3) * q / 2) / 3

distanceFromQuadratic :: Exp (Point2 Float) -> Exp (QuadraticBezier Float) -> Exp Float
distanceFromQuadratic (A.unlift -> p) (A.unlift -> QuadraticBezier p0 p1 p2) = A.cond cond1 --((abs (w2 - w1)) <* 1) --((dot sc sc) <=* 0.001)
    l
    (A.cond (n A.>* 1)
        (min (getLength res0) $ min (getLength res1) (getLength res2))
        (getLength res0))
    where (n :: Exp Int, res :: Exp (Float, Float, Float)) = A.unlift res'
          (res0, res1, res2) = A.unlift res :: (Exp Float, Exp Float, Exp Float)
          res' = solveCubic (b*a) (c*a) (d*a)
          --
          a = sA
          b = sB
          c = sC + dot d' sc
          d = dot d' sd
          --
          d' = p0 - p
          --
          sA = 1 / dot sc sc
          sB = 3 * dot sd sc
          sC = 2 * dot sd sd
          sb = (p1 - p0) `mult` 2
          sc = p0 - (p1 `mult` 2) + p2
          sd = p1 - p0
          --
          getLength r = let
                  t = clamp' 0 1 r
                  pos = p0 + ((sb + (sc `mult` t)) `mult` t)
              in len $ pos - p
          len o = sqrt $ dot o o
          --
          dot (Point2 ox oy) (Point2 qx qy) = ox * qx + oy * qy
          mult x y = fmap (*y) x
          --
          Point2 x1 y1 = p0
          Point2 x2 y2 = p2
          Point2 x  y  = p
          u   = ((x2 - x1) * (x - x1) + (y2 - y1) * (y - y1))/(um1*um1 + um2*um2)
          um1 = x2 - x1
          um2 = y2 - y1
          p3  = Point2 (x1 + u * (x2 - x1)) (y1 + u * (y2 - y1))
          pp3 = len2 p p3
          pp2 = len2 p p2
          pp1 = len2 p p0
          l   = A.cond ((u >* 0) &&* (1 >* u)) pp3 (min pp1 pp2)
          len2 (Point2 x1 y1) (Point2 x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
          --
          cond1 = A.cond ((y3 ==* y1) &&* (y2 ==* y1)) (A.lift True) cond2

          cond2 = (abs(fi1 - fi2)) <* 5
          fi1   = atan ((x3-x1)/(y3-y1))
          fi2   = atan ((x2-x1)/(y2-y1))

          Point2 x3 y3 = p1

          --w1 = (x3 - x1)/(y3 - y1)
          --w2 = (x2 - x1)/(y2 - y1)

distanceFromQuadratic' :: Exp (Point2 Double) -> Exp (QuadraticBezier Double) -> Exp Double
distanceFromQuadratic' _ _ = 1 --(dot sc sc) <=* 0.001)
    --0
    --1 --(getLength res0))
    --where dot (Point2 ox oy) (Point2 qx qy) = ox * qx + oy * qy
          --mult x y = fmap (*y) x
          --(n :: Exp Int, res :: Exp (Double, Double, Double)) = A.unlift res'
          --(res0, res1, res2) = A.unlift res :: (Exp Double, Exp Double, Exp Double)
          --res' = solveCubic (b*a) (c*a) (d*a)
          --a = sA
          --b = sB
          --c = sC + dot d' sc
          --d = dot d' sd
          --d' = p0 - p
          --
          --getLength r = let
          --        t = t clamp' 0 1 r
          --        pos = p0 + ((sb + (sc `mult` t)) `mult` t)
          --    in len $ pos - p
          --len o = sqrt $ dot o o
          --clamp'' low high num = min high (max low num)
          --
          --sA = 1 / dot sc sc
          --sB = 3 * dot sd sc
          --sC = 2 * dot sd sd
          --sb = (p1 - p0) `mult` 2
          --sc = p0 --p0 - (p1 `mult` 2) + p2
          --sd = p1 - p0
          --
          --Point2 x1 y1 = p0
          --Point2 x2 y2 = p2
          --Point2 x  y  = p
          --u   = ((x2 - x1) * (x - x1) + (y2 - y1) * (y - y1))/(um1*um1 + um2*um2)
          --um1 = x2 - x1
          --um2 = y2 - y1
          --p3  = Point2 (x1 + u * (x2 - x1)) (y1 + u * (y2 - y1))
          --pp3 = len2 p p3
          --pp2 = len2 p p2
          --pp1 = len2 p p0
          --l   = A.cond ((u >* 0) &&* (1 >* u)) pp3 (min pp1 pp2)
          --len2 (Point2 x1 y1) (Point2 x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

-- TODO: [KM] make a version of this working on CubicBezier (and doing the conversion to a list of Quadratics inside it)
--distanceFromQuadratics :: Exp (Point2 Float) -> Acc (Vector (QuadraticBezier Float)) -> Exp Float
--distanceFromQuadratics p = A.sfoldl getMin 1e20 A.index0 -- FIXME[KM]: FIX THIS SHIET!
--    where getMin acc curve = min acc $ distanceFromQuadratic p curve

distanceFromQuadratics' :: Exp (Point2 Float) -> Acc(Vector (QuadraticBezier Float)) -> Exp Float
distanceFromQuadratics' p = A.sfoldl getMin 1e20 A.index0
    where getMin acc curve = min acc $ distanceFromQuadratic p curve
