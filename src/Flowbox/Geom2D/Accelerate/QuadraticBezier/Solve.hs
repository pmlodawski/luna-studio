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
          n = sin v' * 1.732050808
          v' = acos w
          w = (-sqrt ((-27) / p3) * q / 2) / 3

almostAlmostCardano :: Exp Float -> Exp Float -> Exp (Int, (Float, Float, Float))
almostAlmostCardano a b = A.cond (a <=* 0.1 &&* a >=* -0.1) res1 res2
    where res1 = A.lift ( 0 :: Int, (0::Float, 0::Float, 0::Float))
          res2 = A.lift ( 0 :: Int, (0::Float, 0::Float, -b/a))

almostCardano :: Exp Float -> Exp Float -> Exp Float -> Exp (Int, (Float, Float, Float))
almostCardano a b c = A.cond (a <=* 0.1 &&* a >=* -0.1) (almostAlmostCardano b c) (A.cond (delta ==* 0) res1 res2)
    where res1 = A.lift ( 0 :: Int, (0::Float, 0::Float, r1))
          res2 = A.lift ( 0 :: Int, (0::Float, r1      , r2))
          delta = b^2 - 4*a*c
          r1 = (-b + (sqrt delta))/(2*a)
          r2 = (-b - (sqrt delta))/(2*a)

cardano :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp (Int, (Float, Float, Float))
cardano a b c d = A.cond (a <=* 0.1 &&* a >=* -0.1) (almostCardano b c d) (A.cond (delta >=* 0) ( A.cond (delta >* 0) opt1 opt2) opt3)
    where opt1     = A.lift ( 1::Int, (opt1y1, 0::Float, 0::Float))
          opt2     = A.lift ( 2::Int, (opt2y1, opt2y2  , 0::Float))
          opt3     = A.lift ( 3::Int, (opt3y1, opt3y2  , opt3y3  ))
          p        = c/a - (b^2)/(3 * a^2)
          q        = (2 * b^3)/(27 * a^3) + d/a - (b * c)/(3 * a^2)
          delta    = (p/3)^3 + (q/2)^2
          opt1y1   = offset $ (cuberoot $ -q/2 - sqrt delta) + (cuberoot $ -q/2 + sqrt delta)
          opt2y1   = offset $ cuberoot $ q/2
          opt2y2   = offset $ -2 * opt2y1
          pi       = 3.14159265359
          fi       = acos $ (-q/2)/(sqrt ((-p^3)/27))
          opt3y1   = offset ( 2 * sqrt (-p/3) * cos ( fi/3 ))
          opt3y2   = offset ( 2 * sqrt (-p/3) * cos ( (fi + 2 * pi)/3 ))
          opt3y3   = offset ( 2 * sqrt (-p/3) * cos ( (fi + 4 * pi)/3 ))
          offset x = x - b/(3 * a)

distanceFromQuadratic' :: Exp (Point2 Float) -> Exp (QuadraticBezier Float) -> Exp Float
distanceFromQuadratic' (A.unlift -> p) (A.unlift -> QuadraticBezier p0 p1 p2) = min (min (min mP0 mP2) (min mPRes0 mPRes1)) mPRes2
    where aA   = p1 - p0
          --Point2 tmp1 tmp2 = p2 - p1 - aA
          tmp3 = p2 - p1
          --Point2 tmp1' tmp2' = p2 + p0 - p1
          bB   = p2 - p1 - aA
          Point2 bb1 bb2 = bB
          Point2 pRes0_1 pRes0_2 = pRes2
          a    = dot bB bB
          b    = 3 * dot aA bB
          m'   = p0 - p
          Point2 aA1 aA2 = aA
          c    = 2 * dot aA aA + dot m' bB
          d    = dot m' aA
          res' = cardano a b c d
          (n :: Exp Int, res :: Exp (Float, Float, Float)) = A.unlift res'
          (res0, res1, res2) = A.unlift res :: (Exp Float, Exp Float, Exp Float)
          mP0    = len p p0
          mP2    = len p p2
          pRes0  = funcP $ clamp' 0 1 res0 :: Point2 (Exp Float)
          pRes1  = funcP $ clamp' 0 1 res1 :: Point2 (Exp Float)
          pRes2  = funcP $ clamp' 0 1 res2 :: Point2 (Exp Float)
          mPRes0 = len p pRes0
          mPRes1 = len p pRes1
          mPRes2 = len p pRes2
          --
          funcP t = (mult p0 ((1-t)^2)) + (mult p1 (2*t*(1-t))) + (mult p2 (t^2)) -- (mult ((1-t)^2) p0) + (mult (2*t*(1-t)) p1) + (mult (t^2) p2) 
          --
          max5 (a, b, c, d, e) = max (max (max a b) (max c d)) e
          len (Point2 x1 y1) (Point2 x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
          dot (Point2 ox oy) (Point2 qx qy) = ox * qx + oy * qy
          mult x y = fmap (*y) x

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

distanceFromQuadratics' :: Exp (Point2 Float) -> Acc(Vector (QuadraticBezier Float)) -> Exp Float
distanceFromQuadratics' p = A.sfoldl getMin 1e20 A.index0
    where getMin acc curve = min acc $ distanceFromQuadratic' p curve

distanceFromQuadratics :: Exp (Point2 Float) -> Acc(Vector (QuadraticBezier Float)) -> Exp Float
distanceFromQuadratics p = error "not implemented yet"
