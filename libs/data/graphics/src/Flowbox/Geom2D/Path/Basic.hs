---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Geom2D.Path.Basic where

import Math.Coordinate.Cartesian (Point2(..))



data PathJoin a = JoinLine a
			    | JoinQuadratic a
