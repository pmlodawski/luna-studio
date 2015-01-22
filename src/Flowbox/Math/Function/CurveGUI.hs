---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Math.Function.CurveGUI where

import qualified Data.Array.Accelerate     as A
import qualified Data.Map                  as Map
import           Math.Coordinate.Cartesian (Point2(..))

import           Flowbox.Geom2D.CubicBezier               as CubicBezier
import           Flowbox.Geom2D.CubicBezier.Solve         as CubicBezier
import           Flowbox.Math.Function.Accelerate.BSpline
import qualified Flowbox.Math.Function.Model              as Model
import           Flowbox.Prelude                          as P

type Weight = Double
type Length = Double
type Angle  = Double

newtype CurvesCollection x = CurvesCollection { _curves :: [(x, Curve x)] } deriving (Show)

data Curve x = BezierCurve { _vertices :: [ControlPoint x] } deriving (Show)

data ControlPoint x = ControlPoint { _point  :: Point2 x
                                   , _handleIn  :: Handle
                                   , _handleOut :: Handle
                                   } deriving (Show, Eq)

data Handle = NonLinear { _weight :: Weight
                        , _angle  :: Angle
                        }
            | Vertical  { _length :: Length
                        }
            | Linear deriving (Show, Eq)

makeLenses ''CurvesCollection
makeLenses ''Curve
makeLenses ''ControlPoint
makeLenses ''Handle

convertToBSpline :: Curve Double -> BSpline Double
convertToBSpline (BezierCurve vertices) = A.fromList (A.Z A.:. (P.length l)) l :: BSpline Double
    where
        l = convertToNodeList vertices

convertToNodeList :: [ControlPoint Double] -> [BSplineNode Double]
convertToNodeList l =
    let
        safeMap :: [ControlPoint Double] -> ControlPoint Double -> [BSplineNode Double] -> [BSplineNode Double]
        safeMap (s:r:seg) l acc = safeMap (r:seg) s ((convertToNode l s r):acc)
        safeMap (s:[]) l acc = reverse ((processRightmost l s):acc)

        reflectPoint :: Point2 Double -> Point2 Double -> Point2 Double
        reflectPoint (Point2 x y) (Point2 x2 y2) = Point2 (2*x - x2) (2*y - y2)

        convertSingleElem :: ControlPoint Double -> [BSplineNode Double]
        convertSingleElem (ControlPoint (Point2 x y) Linear Linear) = [BSplineNode (Point2 x y) (Point2 (x-1) y) (Point2 (x+1) y)]

        convertSingleElem a@(ControlPoint p@(Point2 x y) _ Linear) = [BSplineNode (Point2 x y) l (reflectPoint p l)]
            where
                BSplineNode _ (l@(Point2 lx ly)) _ = processLeftmost a a

        convertSingleElem a@(ControlPoint p@(Point2 x y) Linear _) = [BSplineNode (Point2 x y) (reflectPoint p r) r]
            where
                BSplineNode _ _ (r@(Point2 rx ry)) = processRightmost a a

        convertSingleElem a@(ControlPoint (Point2 x y) _ _) = [BSplineNode (Point2 x y) l r]
            where
                BSplineNode _ l _ = processLeftmost a a
                BSplineNode _ _ r = processRightmost a a

        processLeftmost :: ControlPoint Double -> ControlPoint Double -> BSplineNode Double
        processLeftmost a@(ControlPoint p@(Point2 x y) Linear _) b = BSplineNode (Point2 x y) (reflectPoint p r) r
            where
                r@(Point2 rx ry) = processRight a b

        processLeftmost a@(ControlPoint (Point2 x y) (NonLinear w ang) _) b = BSplineNode (Point2 x y) (Point2 lx ly) r
            where
                r = processRight a b
                lx = x - 1
                ly = y - tan(ang)

        processLeftmost a@(ControlPoint (Point2 x y) _ _) b = BSplineNode (Point2 x y) (processLeft a a) r
            where
                r = processRight a b

        processRightmost :: ControlPoint Double -> ControlPoint Double -> BSplineNode Double
        processRightmost a b@(ControlPoint p@(Point2 x y) _ Linear) = BSplineNode (Point2 x y) l (reflectPoint p l)
            where
                l@(Point2 lx ly) = processLeft a b

        processRightmost a b@(ControlPoint (Point2 x y) _ (NonLinear w ang)) = BSplineNode (Point2 x y) l (Point2 rx ry)
            where
                l = processLeft a b
                rx = x + 1
                ry = y + tan(ang)

        processRightmost a b@(ControlPoint (Point2 x y) _ _) = BSplineNode (Point2 x y) l (processRight b b)
            where
                l = processLeft a b

        convertToNode :: ControlPoint Double -> ControlPoint Double -> ControlPoint Double -> BSplineNode Double
        convertToNode l s@(ControlPoint (Point2 x y) _ _) r =
            BSplineNode (Point2 x y) (processLeft l s) (processRight s r)

        processLeft :: ControlPoint Double -> ControlPoint Double -> Point2 Double
        processLeft (ControlPoint (Point2 x2 y2) _ _) (ControlPoint (Point2 x y) Linear _) = Point2 x' y'
            where
                x' = x - (x - x2) / 3
                y' = y - (y - y2) / 3

        processLeft (ControlPoint (Point2 x2 y2) _ _) (ControlPoint (Point2 x y) (NonLinear w ang) _) = Point2 lx ly
            where
                lx = x - (x - x2) * w
                ly = y - (x - lx) * tan(ang)

        processLeft _ (ControlPoint (Point2 x y) (Vertical w) _) = Point2 x (y+w)

        processRight :: ControlPoint Double -> ControlPoint Double -> Point2 Double
        processRight (ControlPoint (Point2 x y) _ Linear) (ControlPoint (Point2 x2 y2) _ _) = Point2 x' y'
            where
                x' = x + (x2 - x) / 3
                y' = y + (y2 - y) / 3

        processRight (ControlPoint (Point2 x y) _ (NonLinear w ang)) (ControlPoint (Point2 x2 y2) _ _) = Point2 rx ry
            where
                rx = x + (x2 -x) * w
                ry = y + (rx - x) * tan(ang)

        processRight (ControlPoint (Point2 x y) _ (Vertical w)) _ = Point2 x (y+w)
    in
        case l of
            (a:b:seg) -> safeMap (b:seg) a [processLeftmost a b]
            (a:[]) -> convertSingleElem a



valueAtSpline :: Curve Double -> Double -> Double
valueAtSpline (BezierCurve (convertToNodeList -> vertices')) x =
    if vLength < 1
        then 0
        else if x < xL
            then lineValue xL yL xHiL yHiL
            else findValue vertices'
    where vLength  = P.length vertices'
          BSplineNode (Point2 xL yL) (Point2 xHiL yHiL) _ = P.head vertices'
          lineValue xA yA xB yB = let
                  a = (yB - yA) / (xB - xA)
                  b = yA - a * xA
              in if xA == xB
                  then yA
                  else a * x + b
          findValue [BSplineNode (Point2 xR yR) _ (Point2 xHoR yHoR)] = lineValue xR yR xHoR yHoR
          findValue (a:b:xs) = let
                  BSplineNode pA _ hOutA = a
                  BSplineNode pB@(Point2 xB _) hInB _ = b
                  curve = CubicBezier pA hOutA hInB pB
              in if x < xB
                  then CubicBezier.valueAtX 10 0.0001 curve x
                  else findValue $ b:xs
