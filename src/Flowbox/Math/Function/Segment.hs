---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Function.Segment where

import Data.Map as Map

import Flowbox.Math.Function.Model
import Flowbox.Prelude



isValidIndex :: Function -> Int -> Bool
isValidIndex (Function _ rest) idx = idx >= 0 && idx <= Map.size rest

size :: Function -> Int
size (Function _ rest) = Map.size rest + 1

-- TODO: might bea good idea to rescale the adjacent (probably only the previous one) segments so that this one fits in
insertSegmentAt :: Function -> CoordinateX -> Segment -> Function
insertSegmentAt fun x seg = fun & segments %~ Map.insert x seg

getSegmentAt :: Function -> CoordinateX -> Segment
getSegmentAt (Function first rest) x = maybe first snd $ Map.lookupLE x rest

getSegmentIdAt :: Function -> CoordinateX -> Int
getSegmentIdAt (Function _ rest) x =
    let inc = (+1) . flip Map.findIndex rest . fst
    in maybe 0 inc $ Map.lookupLE x rest

getNthSegment :: Function -> Int -> Maybe Segment
getNthSegment fun@(Function first rest) idx = case idx of
    0                      -> Just first
    i | isValidIndex fun i -> Just $ snd $ Map.elemAt (idx - 1) rest
    _                      -> Nothing

replaceSegment :: Function -> Int -> Segment -> Function
replaceSegment (Function _ rest) 0 seg = Function seg rest
replaceSegment fun@(Function first rest) idx seg
    | isValidIndex fun idx = Function first $ Map.updateAt (const . const $ Just seg) (idx - 1) rest
    | otherwise            = fun

modifySegment :: Function -> Int -> (Segment -> Segment) -> Function
modifySegment fun idx f =
    let updatedSegment = f <$> getNthSegment fun idx
    in maybe fun (replaceSegment fun idx) updatedSegment
