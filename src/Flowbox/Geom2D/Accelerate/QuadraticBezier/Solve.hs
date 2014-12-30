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

import Flowbox.Geom2D.Accelerate.QuadraticBezier ()
import Flowbox.Geom2D.QuadraticBezier
import Flowbox.Graphics.Utils.Accelerate         as A
import Flowbox.Graphics.Utils.Utils              hiding (sign)
import Flowbox.Prelude



cuberoot :: Exp Double -> Exp Double
cuberoot x = signum x * pow (abs x)
    where pow = flip (**) (1/3)

solveCubic :: Exp Double -> Exp Double -> Exp Double -> Exp (Int, (Double, Double, Double))
solveCubic a b c = A.cond (d >=* 0) opt1 opt3
    where opt1 = A.lift ( 1::Int, (offset + u + v, 0::Double, 0::Double) )
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
          m = cos v'
          n = sin v' * 1.732050808
          v' = acos (-sqrt ((-27) / p3) * q / 2) / 3

distanceFromQuadratic :: Exp (Point2 Double) -> Exp (QuadraticBezier Double) -> Exp Double
distanceFromQuadratic (A.unlift -> p) (A.unlift -> QuadraticBezier p0 p1 p2) = A.cond (n A.>* 1)
    (min (getLength res0) $ min (getLength res1) (getLength res2))
    (getLength res0)
    where (n :: Exp Int, res :: Exp (Double, Double, Double)) = A.unlift res'
          (res0, res1, res2) = A.unlift res :: (Exp Double, Exp Double, Exp Double)
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

-- TODO: [KM] make a version of this working on CubicBezier (and doing the conversion to a list of Quadratics inside it)
distanceFromQuadratics :: Exp (Point2 Double) -> Acc (Vector (QuadraticBezier Double)) -> Exp Double
distanceFromQuadratics p a = A.sfoldl min 1e20 A.index0 $ A.use $ A.fromList (Z:.3) [0,0.6,0.7]-- A.smap (distanceFromQuadratic p) a
