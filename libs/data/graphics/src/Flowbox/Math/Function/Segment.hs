---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Function.Segment where

import           Control.Monad
import           Data.Map                    as Map

import           Flowbox.Math.Function.Model
import           Flowbox.Prelude



isValidIndex :: FunctionModel x y -> Int -> Bool
isValidIndex (FunctionModel segs) idx = idx >= 0 && idx < Map.size segs

size :: FunctionModel x y -> Int
size (FunctionModel segs) = Map.size segs

insertSegmentAt :: Ord x => FunctionModel x y -> x -> FunctionSegment x y -> FunctionModel x y
insertSegmentAt fun x seg = fun & segments %~ Map.insert x (Just seg)

getSegmentAt :: Ord x => FunctionModel x y -> x -> Maybe (FunctionSegment x y)
getSegmentAt (FunctionModel segs) x = join . fmap snd $ Map.lookupLE x segs

--(x, Maybe (FunctionSegment x y))

getSegmentIdAt :: Ord x => FunctionModel x y -> x -> Maybe Int
getSegmentIdAt (FunctionModel segs) x =
    let getIdx = flip Map.findIndex segs . fst
    in fmap getIdx $ Map.lookupLE x segs

getNthSegment :: Ord x => FunctionModel x y -> Int -> Maybe (FunctionSegment x y)
getNthSegment fun@(FunctionModel segs) idx = case idx of
    i | isValidIndex fun i -> snd $ Map.elemAt idx segs
    _                      -> Nothing

replaceSegment :: Ord x => FunctionModel x y -> Int -> FunctionSegment x y -> FunctionModel x y
replaceSegment fun@(FunctionModel segs) idx seg
    | isValidIndex fun idx = FunctionModel $ Map.updateAt (const . const . pure . pure $ seg) idx segs
    | otherwise            = fun

modifySegment :: Ord x => FunctionModel x y -> Int -> (FunctionSegment x y -> FunctionSegment x y) -> FunctionModel x y
modifySegment fun idx f =
    let updatedSegment = f <$> getNthSegment fun idx
    in maybe fun (replaceSegment fun idx) updatedSegment
