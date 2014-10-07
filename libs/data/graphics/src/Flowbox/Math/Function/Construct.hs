---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Function.Construct where

import Data.List
import Data.Map as Map
import Geom2D

import Flowbox.Math.Function.Model
import Flowbox.Prelude             as P



empty :: Function
empty = Function EmptySegment Map.empty

bsplineFromPoints :: [(Point, Handle, Handle)] -> Segment
bsplineFromPoints points = BSpline $ P.foldl makeNode Map.empty points
    where makeNode acc (Point x y, hIn, hOut) = Map.insert x (Node y hIn hOut) acc

