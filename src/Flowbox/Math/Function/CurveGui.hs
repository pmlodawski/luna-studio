---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Function.CurveGui where

import           Flowbox.Prelude
import           Flowbox.Math.Function.Accelerate.BSpline
import           Math.Coordinate.Cartesian                   (Point2(..))
import qualified Data.Array.Accelerate as A
import qualified Flowbox.Math.Function.Model as Model
import qualified Data.Map as Map

type Weight = Double
type Angle  = Double
data Direction = Up | Down deriving(Show, Eq)

newtype CurvesCollection x = CurvesCollection { _curves :: [(x, Curve x)] } deriving (Show)

data Curve x = BezierCurve { _vertices :: [ControlPoint x] } deriving (Show)

data ControlPoint x = ControlPoint { _point  :: Point2 x
                                   , _handleIn  :: Handle
                                   , _handleOut :: Handle
                                   } deriving (Show, Eq)

data Handle = NonLinear { _weight :: Weight
                        , _angle  :: Angle
                        }
            | Vertical  { _weight :: Weight
                        , _direction :: Direction
                        }
            | Linear deriving (Show, Eq)

makeLenses ''CurvesCollection
makeLenses ''Curve
makeLenses ''ControlPoint
makeLenses ''Handle

convertToBSpline :: Curve Double -> BSpline Double
convertToBSpline (BezierCurve vertices) = A.fromList (A.Z A.:. (length l)) l :: BSpline Double
    where
        l = convertToNodeList vertices

convertToNodeList :: [ControlPoint Double] -> [BSplineNode Double]
convertToNodeList l = 
    let
        safeMap :: [ControlPoint Double] -> ControlPoint Double -> [BSplineNode Double] -> [BSplineNode Double]
        safeMap (s:r:seg) l acc = safeMap (r:seg) s ((convertToNode l s r):acc)
        safeMap (s:[]) l acc = reverse ((processRightmost l s):acc)

        convertSingleElem :: ControlPoint Double -> [BSplineNode Double]
        convertSingleElem (ControlPoint (Point2 x y) Linear Linear) = [BSplineNode (Point2 x y) (Point2 (x-1) y) (Point2 (x+1) y)]

        convertSingleElem a@(ControlPoint (Point2 x y) _ Linear) = [BSplineNode (Point2 x y) l (-l)]
            where
                l = processLeft a a

        convertSingleElem b@(ControlPoint (Point2 x y) Linear _) = [BSplineNode (Point2 x y) (-r) r]
            where
                r = processRight b b

        convertSingleElem a@(ControlPoint (Point2 x y) _ _) = [BSplineNode (Point2 x y) l r]
            where
                l = processLeft a a
                r = processRight a a

        processLeftmost :: ControlPoint Double -> ControlPoint Double -> BSplineNode Double
        processLeftmost a@(ControlPoint (Point2 x y) Linear _) b = BSplineNode (Point2 x y) (-r) r
            where
                r = processRight a b

        processLeftmost a@(ControlPoint (Point2 x y) (NonLinear w ang) _) b = BSplineNode (Point2 x y) (Point2 lx ly) r
            where
                r = processRight a b
                lx = x - 1
                ly = y - tan(ang)

        processLeftmost a@(ControlPoint (Point2 x y) _ _) b = BSplineNode (Point2 x y) (processLeft a a) r
            where
                r = processRight a b

        processRightmost :: ControlPoint Double -> ControlPoint Double -> BSplineNode Double
        processRightmost a b@(ControlPoint (Point2 x y) _ Linear) = BSplineNode (Point2 x y) l (-l)
            where
                l = processLeft a b

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

        processLeft _ (ControlPoint (Point2 x y) (Vertical w d) _) =
            case d of
                Up  -> Point2 x (y+w)
                Down -> Point2 x (y-w)

        processRight :: ControlPoint Double -> ControlPoint Double -> Point2 Double
        processRight (ControlPoint (Point2 x y) _ Linear) (ControlPoint (Point2 x2 y2) _ _) = Point2 x' y'
            where
                x' = x + (x2 - x) / 3
                y' = y + (y2 - y) / 3

        processRight (ControlPoint (Point2 x y) _ (NonLinear w ang)) (ControlPoint (Point2 x2 y2) _ _) = Point2 rx ry
            where
                rx = x + (x2 -x) * w
                ry = y + (rx - x) * tan(ang)

        processRight (ControlPoint (Point2 x y) _ (Vertical w d)) _ =
            case d of
                Up  -> Point2 x (y+w)
                Down -> Point2 x (y-w)
    in
        case l of 
            (a:b:seg) -> safeMap (b:seg) a [processLeftmost a b]
            (a:[]) -> convertSingleElem a
