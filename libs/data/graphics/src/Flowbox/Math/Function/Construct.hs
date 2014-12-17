---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Function.Construct where

import           Data.List
import qualified Data.Map as Map

import Flowbox.Math.Function.Model
import Flowbox.Prelude             as P hiding (empty)



empty :: Function x y
empty = Function Map.empty

fromSegment :: Ord x => x -> Segment x y -> Function x y
fromSegment x seg = Function $ Map.fromList [(x, Just seg)]

hybridCubicFromPoints :: Ord x => [(Point x y, Maybe Handle, Maybe Handle)] -> Segment x y
hybridCubicFromPoints points = ContinuousHybrid $ foldl' makeNode Map.empty points
    where makeNode acc (Point x y, hIn, hOut) = Map.insert x (ControlPoint y hIn hOut) acc

hybridLinearFromPoints :: Ord x => [Point x y] -> Segment x y
hybridLinearFromPoints points = ContinuousHybrid $ foldl' appendPoint Map.empty points
    where appendPoint acc (Point x y) = Map.insert x (hardJoint y) acc

-- TODO: move this to a seperate, appropriately named module
-- TODO: change the type signature to: Function -> Range -> Int -> ( [(CoordinateX, CoordinateY)] OR [Point] )
--linearInterpolation :: Function -> Range -> Int -> Function
--linearInterpolation fun (a, b) n
--    | n < 1  = empty
--    | a > b  = linearInterpolation fun (b, a) n
--    | a == b || n == 1 =
--        let x  = (a+b)/2
--            y' = valueAt fun x
--        in fromSegment $ BSpline $ maybe Map.empty (\y -> Map.fromList [(x, hardJoint y)]) y'
--    | otherwise = undefined
--    -- CO JEŚLI NIE MA WARTOŚCI DLA ZADANEGO X (MAMY NOTHING)? WSTAWIAC ROZNE SEGMENTY? ZEROWAC? POMIJAC PUNKT?
