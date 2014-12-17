---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Geom2D.Conversion where

import           Geom2D

import Math.Coordinate.Cartesian  (Point2(..))
import Flowbox.Prelude


fp2gp :: Point2 Double -> Point
fp2gp (Point2 x y) = Point x y

gp2fp :: Point -> Point2 Double
gp2fp (Point x y) = Point2 x y
