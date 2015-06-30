---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns  #-}

module Flowbox.Math.Function.CurveGUI where

import qualified Data.Array.Accelerate     as A
import           Data.Binary
import           Math.Coordinate.Cartesian (Point2 (..))

import Flowbox.Geom2D.CubicBezier               as CubicBezier
import Flowbox.Geom2D.CubicBezier.Solve         as CubicBezier
import Flowbox.Math.Function.Accelerate.BSpline
import Flowbox.Prelude                          as P


type Weight = Float
type Length = Float
type Angle  = Float

newtype CurvesCollection x = CurvesCollection { _curves :: [(x, CurveGUI x)] } deriving (Show)

data CurveGUI x = BezierCurveGUI { _vertices :: [ControlPointGUI x] } deriving (Generic, Show)

data ControlPointGUI x = ControlPointGUI { _point     :: Point2 x
                                         , _handleIn  :: HandleGUI
                                         , _handleOut :: HandleGUI
                                         } deriving (Eq, Generic, Show)

data HandleGUI = NonLinearGUI { _weight :: Weight
                              , _angle  :: Angle
                              }
               | VerticalGUI  { _length :: Length
                              }
               | LinearGUI deriving (Eq, Generic, Show)

makeLenses ''CurvesCollection
makeLenses ''CurveGUI
makeLenses ''ControlPointGUI
makeLenses ''HandleGUI

instance Binary x => Binary (CurveGUI x)
instance Binary x => Binary (ControlPointGUI x)
instance Binary HandleGUI
instance Binary x => Binary (Point2 x)

convertToBSpline :: CurveGUI Float -> BSpline Float
convertToBSpline (BezierCurveGUI vertices) = A.fromList (A.Z A.:. (P.length l)) l :: BSpline Float
    where
        l = convertToNodeList vertices

convertToNodeList :: [ControlPointGUI Float] -> [BSplineNode Float]
convertToNodeList l =
    let
        safeMap :: [ControlPointGUI Float] -> ControlPointGUI Float -> [BSplineNode Float] -> [BSplineNode Float]
        safeMap (s:r:seg) l acc = safeMap (r:seg) s ((convertToNode l s r):acc)
        safeMap (s:[]) l acc = reverse ((processRightmost l s):acc)

        reflectPoint :: Point2 Float -> Point2 Float -> Point2 Float
        reflectPoint (Point2 x y) (Point2 x2 y2) = Point2 (2*x - x2) (2*y - y2)

        convertSingleElem :: ControlPointGUI Float -> [BSplineNode Float]
        convertSingleElem (ControlPointGUI (Point2 x y) LinearGUI LinearGUI) = [BSplineNode (Point2 x y) (Point2 (x-1) y) (Point2 (x+1) y)]

        convertSingleElem a@(ControlPointGUI p@(Point2 x y) _ LinearGUI) = [BSplineNode (Point2 x y) l (Point2 (lx+1) ly)]
            where
                BSplineNode _ (l@(Point2 lx ly)) _ = processLeftmost a a

        convertSingleElem a@(ControlPointGUI p@(Point2 x y) LinearGUI _) = [BSplineNode (Point2 x y) (Point2 (rx-1) ry) r]
            where
                BSplineNode _ _ (r@(Point2 rx ry)) = processRightmost a a

        convertSingleElem a@(ControlPointGUI (Point2 x y) _ _) = [BSplineNode (Point2 x y) l r]
            where
                BSplineNode _ l _ = processLeftmost a a
                BSplineNode _ _ r = processRightmost a a

        processLeftmost :: ControlPointGUI Float -> ControlPointGUI Float -> BSplineNode Float
        processLeftmost a@(ControlPointGUI p@(Point2 x y) LinearGUI _) b = BSplineNode (Point2 x y) (Point2 (x-1) y) r
            where
                r@(Point2 rx ry) = processRight a b

        processLeftmost a@(ControlPointGUI (Point2 x y) (NonLinearGUI w ang) _) b = BSplineNode (Point2 x y) (Point2 lx ly) r
            where
                r = processRight a b
                lx = x - 1
                ly = y - tan ang

        processLeftmost a@(ControlPointGUI (Point2 x y) _ _) b = BSplineNode (Point2 x y) (processLeft a a) r
            where
                r = processRight a b

        processRightmost :: ControlPointGUI Float -> ControlPointGUI Float -> BSplineNode Float
        processRightmost a b@(ControlPointGUI p@(Point2 x y) _ LinearGUI) = BSplineNode (Point2 x y) l (Point2 (x+1) y)
            where
                l@(Point2 lx ly) = processLeft a b

        processRightmost a b@(ControlPointGUI (Point2 x y) _ (NonLinearGUI w ang)) = BSplineNode (Point2 x y) l (Point2 rx ry)
            where
                l = processLeft a b
                rx = x + 1
                ry = y + tan ang

        processRightmost a b@(ControlPointGUI (Point2 x y) _ _) = BSplineNode (Point2 x y) l (processRight b b)
            where
                l = processLeft a b

        convertToNode :: ControlPointGUI Float -> ControlPointGUI Float -> ControlPointGUI Float -> BSplineNode Float
        convertToNode l s@(ControlPointGUI (Point2 x y) _ _) r =
            BSplineNode (Point2 x y) (processLeft l s) (processRight s r)

        processLeft :: ControlPointGUI Float -> ControlPointGUI Float -> Point2 Float
        processLeft (ControlPointGUI (Point2 x2 y2) _ _) (ControlPointGUI (Point2 x y) LinearGUI _) = Point2 x' y'
            where
                x' = x - (x - x2) / 3
                y' = y - (y - y2) / 3

        processLeft (ControlPointGUI (Point2 x2 y2) _ _) (ControlPointGUI (Point2 x y) (NonLinearGUI w ang) _) = Point2 lx ly
            where
                lx = x - (x - x2) * w
                ly = y - (x - lx) * tan ang

        processLeft _ (ControlPointGUI (Point2 x y) (VerticalGUI w) _) = Point2 x (y+w)

        processRight :: ControlPointGUI Float -> ControlPointGUI Float -> Point2 Float
        processRight (ControlPointGUI (Point2 x y) _ LinearGUI) (ControlPointGUI (Point2 x2 y2) _ _) = Point2 x' y'
            where
                x' = x + (x2 - x) / 3
                y' = y + (y2 - y) / 3

        processRight (ControlPointGUI (Point2 x y) _ (NonLinearGUI w ang)) (ControlPointGUI (Point2 x2 y2) _ _) = Point2 rx ry
            where
                rx = x + (x2 -x) * w
                ry = y + (rx - x) * tan ang

        processRight (ControlPointGUI (Point2 x y) _ (VerticalGUI w)) _ = Point2 x (y+w)
    in
        case l of
            (a:b:seg) -> safeMap (b:seg) a [processLeftmost a b]
            (a:[])    -> convertSingleElem a
            []        -> []


valueAtSpline :: CurveGUI Float -> Float -> Float
valueAtSpline (BezierCurveGUI (convertToNodeList -> vertices')) x =
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
