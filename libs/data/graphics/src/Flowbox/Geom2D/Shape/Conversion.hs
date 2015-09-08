---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Geom2D.Shape.Conversion where

import Data.Maybe

import Flowbox.Geom2D.QuadraticBezier
import Flowbox.Geom2D.QuadraticBezier.Conversion
import Flowbox.Geom2D.CubicBezier
import Flowbox.Geom2D.Shape
import Flowbox.Geom2D.Path
import Flowbox.Geom2D.ControlPoint
import Flowbox.Prelude
import Math.Coordinate.Cartesian (Point2(..))



toQuadratics :: Shape Float -> [QuadraticPath Float]
toQuadratics (Shape paths) = fmap makeQuadratics paths
    where makeQuadratics (Path closed points) = (closed, convertCubicsToQuadratics 10 0.001 $ makeCubics points closed)
          makeCubics points closed = foldr appendCubic (ifClosed points closed) $ segments points
          appendCubic pointPair cubics = makeCubic pointPair : cubics
          extract = fromMaybe (Point2 0 0)
          makeCubic (ControlPoint pA _ (extract -> hA), ControlPoint pB (extract -> hB) _) = CubicBezier pA (pA+hA) (pB+hB) pB
          ifClosed points closed = case closed of
              False -> []
              True  -> [makeCubic (last points, head points)]
          segments points = zip points $ tail points
