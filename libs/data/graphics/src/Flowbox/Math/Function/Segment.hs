---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Function.Segment where

import Control.Monad
import Data.Map as Map

import Flowbox.Math.Function.Model
import Flowbox.Prelude



isValidIndex :: Function x y -> Int -> Bool
isValidIndex (Function segs) idx = idx >= 0 && idx < Map.size segs

size :: Function x y -> Int
size (Function segs) = Map.size segs

insertSegmentAt :: Ord x => Function x y -> x -> Segment x y -> Function x y
insertSegmentAt fun x seg = fun & segments %~ Map.insert x (Just seg)

getSegmentAt :: Ord x => Function x y -> x -> Maybe (Segment x y)
getSegmentAt (Function segs) x = join . fmap snd $ Map.lookupLE x segs

--(x, Maybe (Segment x y))

getSegmentIdAt :: Ord x => Function x y -> x -> Maybe Int
getSegmentIdAt (Function segs) x =
    let getIdx = flip Map.findIndex segs . fst
    in fmap getIdx $ Map.lookupLE x segs

getNthSegment :: Ord x => Function x y -> Int -> Maybe (Segment x y)
getNthSegment fun@(Function segs) idx = case idx of
    i | isValidIndex fun i -> snd $ Map.elemAt idx segs
    _                      -> Nothing

replaceSegment :: Ord x => Function x y -> Int -> Segment x y -> Function x y
replaceSegment fun@(Function segs) idx seg
    | isValidIndex fun idx = Function $ Map.updateAt (const . const . pure . pure $ seg) idx segs
    | otherwise            = fun

modifySegment :: Ord x => Function x y -> Int -> (Segment x y -> Segment x y) -> Function x y
modifySegment fun idx f =
    let updatedSegment = f <$> getNthSegment fun idx
    in maybe fun (replaceSegment fun idx) updatedSegment
