---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Function.Solve where

import Data.Map

import Math.Coordinate.Cartesian         (Point2(..))
import Flowbox.Geom2D.CubicBezier
import Flowbox.Geom2D.CubicBezier.Solve
import Flowbox.Math.Function.Model             hiding (Point)
import Flowbox.Prelude



--valueAt :: Function -> CoordinateX -> Maybe CoordinateY
--valueAt fun x = valueAtSegment (getSegmentAt fun x) x

-- INFO: this is set to Double because of line 37, where the types must match for the calculations to work
valueAtSegment :: Segment Double Double -> Double -> Maybe Double
valueAtSegment (ContinuousHybrid nodes) x = result
    where startNode = x `lookupLE` nodes
          endNode   = x `lookupGT` nodes
          result = case (startNode, endNode) of
              -- no nodes
              (Nothing, Nothing)     -> Nothing
              -- before the first node
              (Nothing, Just end)    ->
                  let (nodeX, ControlPoint nodeY hIn _) = end
                      a = case hIn of
                              Nothing -> 0 -- TODO[km]: this has to be calculated according to the other side of the control point # isLinear => Handle == Nothing => (-Inf, nodeX) is a linear continuation of the func
                              Just (Handle _ a') -> a'
                  in Just $ nodeY + (x - nodeX) * tan a
              -- after the last node
              (Just start, Nothing)  ->
                  let (nodeX, ControlPoint nodeY _ hOut) = start
                      a = case hOut of
                              Nothing -> 0 -- TODO[km]: same as the one from -Inf to the first node
                              Just (Handle _ a') -> a'
                  in Just $ nodeY + (x - nodeX) * tan a
              -- somewhere on the BSpline
              (Just (xA, ControlPoint yA _ hA), Just (xB, ControlPoint yB hB _)) ->
                  let valueOf curve = valueAtX 10 0.000001 curve x
                      dx = abs $ xB - xA
                      dy = abs $ yB - yA
                  in case (hA, hB) of
                      (Nothing, Nothing) ->
                          Just $ (x - xA) / (xB - xA) * (yB - yA)
                      _ ->
                          let (nxA, nyA, nxB, nyB) = case (hA, hB) of
                                  (Just (Handle wA aA), Nothing) ->
                                      let nxA' = wA * dx in (nxA', nxA' * sin aA, 1/3 * dx, 1/3 * dy)
                                  (Nothing, Just (Handle wB aB)) ->
                                      let nxB' = wB * dx in (1/3 * dx, 1/3 * dy, nxB', nxB' * sin aB)
                                  (Just (Handle wA aA), Just (Handle wB aB))  ->
                                      let nxA' = wA * dx
                                          nxB' = wB * dx
                                      in (nxA', nxA' * sin aA, nxB', nxB' * sin aB)
                                  (Nothing, Nothing) -> error "how the heck didn't the previous match catch it?"
                              (xC, yC, xD, yD) = (xA + nxA, yA + nyA, xB - nxB, yB - nyB)
                          in Just $ valueOf $ CubicBezier (Point2 xA yA) (Point2 xC yC) (Point2 xD yD) (Point2 xB yB)
valueAtSegment (Lambda f) x = Just $ f x
--valueAtSegment (Repeater startX fun (from, to)) x = undefined
valueAtSegment _ _ = Nothing
