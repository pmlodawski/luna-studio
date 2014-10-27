---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Geom2D.Shape.Conversion where

import Flowbox.Geom2D.QuadraticBezier
import Flowbox.Geom2D.QuadraticBezier.Conversion
import Flowbox.Geom2D.CubicBezier
import Flowbox.Geom2D.Shape
import Flowbox.Geom2D.Path
import Flowbox.Geom2D.ControlPoint
import Flowbox.Prelude



toQuadratics :: Shape Double -> [QuadraticPath Double]
toQuadratics (Shape paths) = fmap makeQuadratics paths
    where makeQuadratics (Path points closed) = (closed, convertCubicsToQuadratics 10 0.001 $ makeCubics points closed)
          makeCubics points closed = foldr appendCubic (ifClosed points closed) $ segments points
          appendCubic pointPair cubics = makeCubic pointPair : cubics
          makeCubic (ControlPoint pA _ hA, ControlPoint pB hB _) = CubicBezier pA (pA+hA) (pB+hB) pB
          ifClosed points closed = case closed of
              False -> []
              True  -> [makeCubic (last points, head points)]
          segments points = zip points $ tail points
