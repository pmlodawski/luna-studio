---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Function.Query where

import Data.Map
import Geom2D
import Geom2D.CubicBezier.Basic

import Flowbox.Geom2D.CubicBezier.Intersection
import Flowbox.Math.Function.Model
import Flowbox.Math.Function.Segment
import Flowbox.Prelude



valueAt :: Function -> CoordinateX -> Maybe CoordinateY
valueAt fun x = valueAtSegment (getSegmentAt fun x) x

valueAtSegment :: Segment -> CoordinateX -> Maybe CoordinateY
valueAtSegment (BSpline nodes) x = result
    where startNode = x `lookupLE` nodes
          endNode   = x `lookupGT` nodes
          coords theta r = (r * cos theta, r * sin theta)
          result = case (startNode, endNode) of
              -- no nodes
              (Nothing, Nothing)     -> Nothing
              -- before the first node
              (Nothing, Just end)    ->
                  let (nodeX, Node nodeY (Handle _ a) _) = end
                  in Just $ nodeY + (x - nodeX) * tan a
              -- after the last node
              (Just start, Nothing)  ->
                  let (nodeX, Node nodeY _ (Handle _ a)) = start
                  in Just $ nodeY + (x - nodeX) * tan a
              -- somewhere on the BSpline
              (Just start, Just end) ->
                  let (xA, Node yA _ (Handle wA aA)) = start
                      (xB, Node yB (Handle wB aB) _) = end
                      dx = xB - xA
                      (xC, yC) = coords aA $ wA * dx
                      (xD, yD) = coords aB $ (-wB) * dx
                  in Just $ valueAtX 10 0.000001 (CubicBezier (Point xA yA) (Point xC yC) (Point xD yD) (Point xB yB)) x
valueAtSegment EmptySegment _ = Nothing
valueAtSegment _ _ = Nothing
