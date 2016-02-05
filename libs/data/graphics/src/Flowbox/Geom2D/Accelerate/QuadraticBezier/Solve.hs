---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve where

import           Data.Array.Accelerate                     as A
import           Math.Coordinate.Cartesian                 (Point2 (..))

import           Flowbox.Geom2D.Accelerate.QuadraticBezier ()
import           Flowbox.Geom2D.QuadraticBezier
import qualified Flowbox.Graphics.Utils.Accelerate         as A
import           Flowbox.Graphics.Utils.Utils              hiding (sign)
import           Flowbox.Prelude                           hiding ((<*))
import qualified Flowbox.Prelude                           as P hiding ((<*))

import qualified Flowbox.Math.Matrix                       as M



cuberoot :: Exp Float -> Exp Float
cuberoot x = signum x * pow (abs x)
    where pow = flip (**) (1/3)

almostAlmostCardano :: Exp Float -> Exp Float -> Exp (Int, (Float, Float, Float))
almostAlmostCardano a b = A.cond (a <=* 0.01 &&* a >=* -0.01) res1 res2
    where res1 = A.lift ( 0 :: Int, (0::Float, 0::Float, 0::Float))
          res2 = A.lift ( 0 :: Int, (0::Float, 0::Float, -b/a))

almostCardano :: Exp Float -> Exp Float -> Exp Float -> Exp (Int, (Float, Float, Float))
almostCardano a b c = A.cond (a <=* 0.01 &&* a >=* -0.01) (almostAlmostCardano b c) (A.cond (delta ==* 0) res1 res2)
    where res1 = A.lift ( 0 :: Int, (0::Float, 0::Float, r1))
          res2 = A.lift ( 0 :: Int, (0::Float, r1      , r2))
          delta = b^2 - 4*a*c
          r1 = (-b + (sqrt delta))/(2*a)
          r2 = (-b - (sqrt delta))/(2*a)

cardano :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp (Int, (Float, Float, Float))
cardano a b c d = A.cond (a <=* 0.01 &&* a >=* -0.01) (almostCardano b c d) (A.cond (delta >=* 0) ( A.cond (delta >* 0) opt1 opt2) opt3)
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
          bB   = p2 - p1 - aA
          a    = dot bB bB
          b    = 3 * dot aA bB
          m'   = p0 - p
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
          funcP t = (mult p0 ((1-t)^2)) + (mult p1 (2*t*(1-t))) + (mult p2 (t^2))
          --
          max5 (a, b, c, d, e) = max (max (max a b) (max c d)) e
          len (Point2 x1 y1) (Point2 x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
          dot (Point2 ox oy) (Point2 qx qy) = ox * qx + oy * qy
          mult x y = fmap (*y) x

distanceFromQuadratics' :: Exp (Point2 Float) -> Acc(Vector (QuadraticBezier Float)) -> Exp Float
distanceFromQuadratics' p = A.sfoldl getMin 1e20 A.index0
    where getMin acc curve = min acc $ distanceFromQuadratic' p curve

distanceFromQuadratics2 :: Exp (Point2 Float) -> Acc(Vector (QuadraticBezier Float)) -> Exp (Float, Float)
distanceFromQuadratics2 p = A.sfoldl getMin (A.lift (1e20::Float, 1e20::Float)) A.index0
    where getMin :: Exp (Float, Float) -> Exp (QuadraticBezier Float) -> Exp (Float, Float)
          getMin acc curve = ( (A.lift (0::Float,0::Float)))
          dc curve = distanceFromQuadratic' p curve
    --where getMin (p1, p2) curve = (A.cond (dc <* p1) () (),0::Float)
          --dc = distanceFromQuadratic' p curve
