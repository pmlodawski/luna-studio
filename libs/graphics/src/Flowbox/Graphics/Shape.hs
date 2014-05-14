---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}

module Flowbox.Graphics.Shape where

import           Data.Array.Accelerate (Z(..))
import qualified Data.Array.Accelerate as A
--import qualified Data.Array.IArray     as IA
import           Data.Monoid           as Monoid

import qualified Math.BernsteinPoly              as Bernstein
import           Geom2D                          (Point(..),Line(..))
import qualified Geom2D                          as G
import           Geom2D.CubicBezier.Basic        (CubicBezier(..), PathJoin(..), Path(..))
import qualified Geom2D.CubicBezier.Basic        as Bezier
import qualified Geom2D.CubicBezier.Intersection as Bezier

import           Flowbox.Prelude                as P
import           Flowbox.Graphics.Image         (Image)
import qualified Flowbox.Graphics.Image         as Image
import           Flowbox.Graphics.Image.Channel (Channel2)
import qualified Flowbox.Graphics.Image.Channel as Channel

--import qualified Debug.Trace as Dbg

data Segment = SegLine Line
             | SegCurve CubicBezier

data Rectangle = Rectangle Point Point deriving (Show)
type BoundingBox = Rectangle

data Size2 = Width Double
           | Height Double
           | Dims Double Double

instance Monoid BoundingBox where
    mempty = Rectangle (Point 0 0) (Point 0 0)
    (Rectangle (Point minAx minAy) (Point maxAx maxAy)) `mappend` (Rectangle (Point minBx minBy) (Point maxBx maxBy)) =
        Rectangle (Point (min minAx minBx) (min minAy minBy)) (Point (max maxAx maxBx) (max maxAy maxBy))

boundingBoxWidth :: BoundingBox -> Double
boundingBoxWidth (Rectangle (Point xA _) (Point xB _)) = abs $ xA - xB

boundingboxHeight :: BoundingBox -> Double
boundingboxHeight (Rectangle (Point _ yA) (Point _ yB)) = abs $ yA - yB

distanceFromPath :: Path -> Point -> Double -> Double
distanceFromPath path point eps = minimum closestPerSegment
    where closestPerSegment = getClosest <$> segments path
          getClosest (SegLine line) = G.lineDistance line point
          getClosest (SegCurve curve) = foldr min minToEnd distances -- FIXME: change this after the `cubicbezier` package gets updated with the fix for `Bezier.closest` returning no results
                where minToEnd = min (distance start) (distance end)
                      distance = G.vectorDistance point
                      CubicBezier start _ _ end = curve
                      distances = fmap (distance . Bezier.evalBezier curve)
                                       (Bezier.closest curve point eps)

scale :: Path -> (Double, Double) -> Path
scale path ratio = case path of
    OpenPath nodes end -> OpenPath (fmap scaleNode nodes) (scalePoint end)
    ClosedPath nodes -> ClosedPath $ fmap scaleNode nodes
    where scaleNode (point, joint) = (scalePoint point, scaleJoint joint)
          scalePoint (Point x y) = Point (fst ratio * x) (snd ratio * y)
          scaleJoint joint = case joint of
              JoinLine -> JoinLine
              JoinCurve a b -> JoinCurve (scalePoint a) (scalePoint b)

segments :: Path -> [Segment]
segments path = case path of
    (OpenPath nodes p)           -> segments' nodes p
    (ClosedPath [])              -> []
    (ClosedPath nodes@((p,_):_)) -> segments' nodes p
    where segments' [] _                 = []
          segments' (n:[]) lp            = [makeSegment n lp]
          segments' nodes@(n:(p,_):_) lp = makeSegment n p : segments' (tail nodes) lp
          makeSegment (a, JoinLine) b          = SegLine  $ Line a b
          makeSegment (a, JoinCurve c1 c2) b   = SegCurve $ CubicBezier a c1 c2 b

findDerivRoots :: CubicBezier -> Double -> Double -> Double -> [Double]
findDerivRoots curve boundLo boundHi eps = uncurry (++) roots
    where roots    = over each bfr bern'
          bfr poly = Bezier.bezierFindRoot poly boundLo boundHi eps
          bern' = over each Bernstein.bernsteinDeriv bern
          bern = Bezier.bezierToBernstein curve

bezierBoundingBox :: CubicBezier -> Double -> BoundingBox
bezierBoundingBox curve eps = Rectangle pointMin pointMax
    where pointMin = Point (getCoord minimum pointX) (getCoord minimum pointY)
          pointMax = Point (getCoord maximum pointX) (getCoord maximum pointY)
          getCoord f coord = f $ fmap coord points
          points   = pointA:pointB:roots
          CubicBezier pointA _ _ pointB = curve
          roots    = Bezier.evalBezier curve <$> findDerivRoots curve 0 1 eps

rasterizeMask :: (Image img (Channel2 Double))
    => Int -> Int -> Double -> Double -> Size2 -> Path -> img (Channel2 Double)
rasterizeMask width height x y size path = Image.insert "rgba.a" alpha mempty -- TODO: make a use of x and y
    where alpha     = Channel.Acc $ A.use $ A.fromList (Z A.:. height A.:. width) makeMask
          --alpha     = Channel.Acc $ A.use $ A.fromIArray makeMask
          --makeMask  = IA.array (0, num) [(i, makeElement i) | i <- [0..num]]
          makeMask  = [makeElement i | i <- [0..num]]
          makeElement i = let iy = i `div` width
                              ix = i - iy * width
                              y' = fromIntegral iy
                              x' = fromIntegral ix
                          in 1 - distanceFromPath pathScaled (Point x' y') eps / fromIntegral (max width height)
          num        = width * height
          pathScaled = scale path scaleRatio
          scaleRatio = case size of
              Width  w -> let ratio = w / boundingBoxW  in (ratio, ratio)
              Height h -> let ratio = h / boundingBoxH in (ratio, ratio)
              Dims w h -> (w / boundingBoxW, h / boundingBoxH)
          boundingBoxW = boundingBoxWidth  boundingBox
          boundingBoxH = boundingboxHeight boundingBox
          boundingBox  = Monoid.mconcat $ fmap bb segs
          bb segment   = case segment of
              SegLine (Line (Point xA yA) (Point xB yB)) -> Rectangle (Point (min xA xB) (min yA yB)) (Point (max xA xB) (max yA yB))
              SegCurve curve -> bezierBoundingBox curve eps
          segs = segments path
          eps  = 0.001
