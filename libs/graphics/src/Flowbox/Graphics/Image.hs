---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image (
    module Flowbox.Graphics.Image,
    Error(..),
    Result
) where

import           Control.Error
import           Data.Array.Accelerate             (Exp)
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Repa                   as R
import           Data.Map                          (Map)
import qualified Data.Map                          as Map

import           Flowbox.Graphics.Image.Channel (ChannelAcc, Channel, Channel2, RawData2)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Error   (Error (ChannelLookupError), Result)
import           Flowbox.Graphics.Transform     (Transformation(..), Transformed(..))
import           Flowbox.Prelude                hiding (lookup, map)



class (Monoid (img a), Functor img) => Image img a where
    channels :: Lens (img a) (img b) (Map Channel.Name a) (Map Channel.Name b)

type Stencil3x1 a = (A.Stencil3 a, A.Stencil3 a, A.Stencil3 a)
type Stencil1x3 a = (A.Stencil3 a, A.Stencil3 a, A.Stencil3 a)


-- ==== Computation

compute :: (Image img (ChannelAcc ix a)) => Channel.Backend ix a -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
compute backend img = img & channels %~ Map.map (Channel.compute backend)


-- ==== Traversal

-- == Map

map :: (Image img (ChannelAcc ix a), Image img (ChannelAcc ix b))
    => (ChannelAcc ix a -> ChannelAcc ix b) -> img (ChannelAcc ix a) -> img (ChannelAcc ix b)
map f img = img & channels %~ Map.map f

mapChannels :: (Image img (ChannelAcc ix a), Image img (ChannelAcc ix b), A.Shape ix, A.Elt a, A.Elt b)
    => (Exp a -> Exp b) -> img (ChannelAcc ix a) -> img (ChannelAcc ix b)
mapChannels f img = img & channels %~ Map.map (Channel.map f)

mapWithKey :: (Image img (ChannelAcc ix a), Image img (ChannelAcc ix b))
    => (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b) -> img (ChannelAcc ix a) -> img (ChannelAcc ix b)
mapWithKey f img = img & channels %~ Map.mapWithKey f

-- TODO: think about how to make this function work in regard to mapWithKey in a similar way map' works in reagard to map
--mapWithKey' :: (A.Shape ix, A.Elt a, A.Elt b) => (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b) -> img (A.Array ix a) -> img (A.Array ix b)
--mapWithKey' f img = Image $ Map.mapWithKey f $ view channels img

-- TODO: make mapAccum using lenses (without the `Image` constructor)
--mapAccum :: (a -> ChannelAcc ix b -> (a, ChannelAcc ix c)) -> a -> img (A.Array ix b) -> (a, img (A.Array ix c))
--mapAccum f acc img = (fst result, Image $ snd result)
    --where result = Map.mapAccum f acc (img ^. channels)

-- TODO: make mapAccumWithKey using lenses (without the `Image` constructor)
--mapAccumWithKey :: (a -> Channel.Name -> ChannelAcc ix b -> (a, ChannelAcc ix c)) -> a -> img (A.Array ix b) -> (a, img (A.Array ix c))
--mapAccumWithKey f acc img = (fst result, Image $ snd result)
--    where result = Map.mapAccumWithKey f acc (img ^. channels)


-- == Folds

foldr :: Image img (ChannelAcc ix a)
    => (ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix b) -> ChannelAcc ix b -> img (ChannelAcc ix a) -> ChannelAcc ix b
foldr f acc img = Map.foldr f acc $ view channels img

foldl :: Image img (ChannelAcc ix b)
    => (ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix a) -> ChannelAcc ix a -> img (ChannelAcc ix b) -> ChannelAcc ix a
foldl f acc img = Map.foldl' f acc $ view channels img

foldrWithKey :: Image img (ChannelAcc ix a)
    => (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix b) -> ChannelAcc ix b -> img (ChannelAcc ix a) -> ChannelAcc ix b
foldrWithKey f acc img = Map.foldrWithKey f acc $ view channels img

foldlWithKey :: Image img (ChannelAcc ix b)
    => (ChannelAcc ix a -> Channel.Name -> ChannelAcc ix b -> ChannelAcc ix a) -> ChannelAcc ix a -> img (ChannelAcc ix b) -> ChannelAcc ix a
foldlWithKey f acc img = Map.foldlWithKey f acc $ view channels img


-- ==== Conversion

elems :: Image img (ChannelAcc ix a) => img (ChannelAcc ix a) -> [ChannelAcc ix a]
elems img = img ^. channels . to Map.elems

keys :: Image img (ChannelAcc ix a) => img (ChannelAcc ix a) -> [Channel.Name]
keys img = img ^. channels . to Map.keys

assocs :: Image img (ChannelAcc ix a) => img (ChannelAcc ix a) -> [(Channel.Name, ChannelAcc ix a)]
assocs img = img ^. channels . to Map.assocs


-- == Lists

-- TODO: think of a better way to get the info about the required image type (ie. ImageRGBA) than by passing an object of a similar type to the function
fromList :: Image img (ChannelAcc ix a) => img (ChannelAcc ix a) -> [(Channel.Name, ChannelAcc ix a)] -> img (ChannelAcc ix a)
fromList img list = img & channels .~ Map.fromList list


-- ==== Accessors


-- == Getters

elementAt :: Image img (ChannelAcc ix a) => Int -> img (ChannelAcc ix a) -> (Channel.Name, ChannelAcc ix a)
elementAt pos img = Map.elemAt pos $ view channels img

get :: Image img (ChannelAcc ix a) => Channel.Name -> img (ChannelAcc ix a) -> Result (ChannelAcc ix a)
get cname img = justErr (ChannelLookupError cname) $ Map.lookup cname (view channels img)


-- == Setters

insert :: Image img (ChannelAcc ix a) => Channel.Name -> ChannelAcc ix a -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
insert cname chan img = img & channels %~ Map.insert cname chan

remove :: Image img (ChannelAcc ix a) => Channel.Name -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
remove cname img = img & channels %~ Map.delete cname

adjust :: Image img (ChannelAcc ix a) => (ChannelAcc ix a -> ChannelAcc ix a) -> String -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
adjust f cname img = img & channels %~ Map.adjust f cname

adjustWithKey :: Image img (ChannelAcc ix a) => (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix a) -> String -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
adjustWithKey f cname img = img & channels %~ Map.adjustWithKey f cname

update :: Image img (ChannelAcc ix a) => (ChannelAcc ix a -> Maybe (ChannelAcc ix a)) -> Channel.Name -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
update f cname img = img & channels %~ Map.update f cname

updateWithKey :: Image img (ChannelAcc ix a) => (Channel.Name -> ChannelAcc ix a -> Maybe (ChannelAcc ix a)) -> String -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
updateWithKey f cname img = img & channels %~ Map.updateWithKey f cname

alter :: Image img (ChannelAcc ix a) => (Maybe (ChannelAcc ix a) -> Maybe (ChannelAcc ix a)) -> Channel.Name -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
alter f cname img = img & channels %~ Map.alter f cname

cpChannel :: Image img (ChannelAcc ix a) => Channel.Name -> Channel.Name -> img (ChannelAcc ix a) -> Result (img (ChannelAcc ix a))
cpChannel source destination img = do
    chan <- get source img
    return $ img & channels %~ Map.insert destination chan


-- ==== Combine

-- == Union

channelUnion :: Image img (ChannelAcc ix a) => img (ChannelAcc ix a) -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
channelUnion imgA imgB = imgA & channels .~ Map.union (imgA ^. channels) (imgB ^. channels)

channelUnionWith :: Image img (ChannelAcc ix a) => (ChannelAcc ix a -> ChannelAcc ix a -> ChannelAcc ix a) -> img (ChannelAcc ix a) -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
channelUnionWith f imgA imgB = imgA & channels .~ Map.unionWith f (imgA ^. channels) (imgB ^. channels)

channelUnionWithKey :: Image img (ChannelAcc ix a) => (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix a -> ChannelAcc ix a) -> img (ChannelAcc ix a) -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
channelUnionWithKey f imgA imgB = imgA & channels .~ Map.unionWithKey f (imgA ^. channels) (imgB ^. channels)


-- == Intersection

channelIntersection :: (Image img (ChannelAcc ix a), Image img (ChannelAcc ix b))
    => img (ChannelAcc ix a) -> img (ChannelAcc ix b) -> img (ChannelAcc ix a)
channelIntersection imgA imgB = imgA & channels .~ Map.intersection (imgA ^. channels) (imgB ^. channels)

channelIntersectionWith :: (Image img (ChannelAcc ix a), Image img (ChannelAcc ix b), Image img (ChannelAcc ix c))
    => (ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix c) -> img (ChannelAcc ix a) -> img (ChannelAcc ix b) -> img (ChannelAcc ix c)
channelIntersectionWith f imgA imgB = imgA & channels .~ Map.intersectionWith f (imgA ^. channels) (imgB ^. channels)

channelIntersectionWithKey :: (Image img (ChannelAcc ix a), Image img (ChannelAcc ix b), Image img (ChannelAcc ix c))
    => (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix c) -> img (ChannelAcc ix a) -> img (ChannelAcc ix b) -> img (ChannelAcc ix c)
channelIntersectionWithKey f imgA imgB = imgA & channels .~ Map.intersectionWithKey f (imgA ^. channels) (imgB ^. channels)


-- ==== Filter

-- TODO: filter, filterWithKey/filterWithName : naive implementation using Map.toAscList and Map.fromAscList - boolean problem
-- and implement filterByName using filterWithName

-- INFO: version without ' filters out existing channels, version with ' enforces the existance of the channels listed on the list
filterByName :: Image img (ChannelAcc ix a) => [Channel.Name] -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
filterByName names img = img & channels %~ Map.filterWithKey nameMatches
    where nameMatches cname _ = cname `elem` names

filterByName' :: Image img (ChannelAcc ix a) => [Channel.Name] -> img (ChannelAcc ix a) -> Result (img (ChannelAcc ix a))
filterByName' names img = do
    channelList <- mapM makePair names
    return $ fromList img channelList
    where makePair name = do
              chan <- get name img
              return (name, chan)

elemsByName' :: Image img (ChannelAcc ix a) => [Channel.Name] -> img (ChannelAcc ix a) -> Result [ChannelAcc ix a]
elemsByName' names img = mapM (`get` img) names

assocsByName' :: Image img (ChannelAcc ix a) => [Channel.Name] -> img (ChannelAcc ix a) -> Result [(Channel.Name, ChannelAcc ix a)]
assocsByName' names img = mapM makePair names
                          where makePair name = do
                                    chan <- get name img
                                    return (name, chan)

selectChannels :: Image img (ChannelAcc ix a) => Channel.Select -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
selectChannels channelList img = case channelList of
    Channel.AllChannels      -> img
    Channel.ChannelList list -> filterByName list img

-- ==== Conversion between numeric types

toFloat :: (A.Shape ix, Image img (ChannelAcc ix A.Word8), Image img (ChannelAcc ix Float))
    => img (ChannelAcc ix A.Word8) -> img (ChannelAcc ix A.Float)
toFloat = mapChannels (\c -> A.fromIntegral c / 255)

toDouble :: (A.Shape ix, Image img (ChannelAcc ix A.Word8), Image img (ChannelAcc ix Double))
    => img (ChannelAcc ix A.Word8) -> img (ChannelAcc ix A.Double)
toDouble = mapChannels (\c -> A.fromIntegral c / 255)

toFloating :: (A.Shape ix, A.Elt a, Image img (ChannelAcc ix A.Word8), Image img (ChannelAcc ix a))
    => A.IsFloating a => img (ChannelAcc ix A.Word8) -> img (ChannelAcc ix a)
toFloating = mapChannels (\c -> A.fromIntegral c / 255)

toWord8 :: (Image img (ChannelAcc ix a), Image img (ChannelAcc ix A.Word8))
    => (A.Shape ix, A.Elt a, A.IsFloating a) => img (ChannelAcc ix a) -> img (ChannelAcc ix A.Word8)
toWord8 = mapChannels (\c -> A.truncate $ c * 255)



-- ==== TRANSFORMATIONS

-- FIXME: find a better way of resampling after rasterization
-- czysty kod kurwa
rasterizeChannel :: (A.Elt a, A.IsFloating a) => Transformation -> Channel2 a -> Channel2 a
rasterizeChannel (Transformation t) ch =
    Channel.stencil (convolve3x1 initialKernel) A.Clamp $
    Channel.stencil (convolve1x3 initialKernel) A.Clamp $
    Channel.generate sh f
    where initialKernel = [1 / 6, 2 / 3, 1 / 6]
          convolve3x1 :: (A.Elt a, A.IsNum a) => [Exp a] -> Stencil3x1 a -> Exp a
          convolve3x1 kernel (_, (a,b,c), _) = sum $ zipWith (*) kernel [a,b,c]
          convolve1x3 :: (A.Elt a, A.IsNum a) => [Exp a] -> Stencil1x3 a -> Exp a
          convolve1x3 kernel ((_,a,_), (_,b,_), (_,c,_)) = sum $ zipWith (*) kernel [a,b,c]
          sh = Channel.shape ch
          (A.Z A.:. y A.:. x) = A.unlift sh
          inShape b a = ((a A.>=* 0) A.&&* (a A.<* x)) A.&&* ((b A.>=* 0) A.&&* (b A.<* y))
          f idx = let
                    (A.Z A.:. j A.:. i) = A.unlift idx
                    -- FIXME: use the conversion available in accelerate-io
                    mat = ( A.constant $ t R.! (R.Z R.:. 0 R.:. 0)
                          , A.constant $ t R.! (R.Z R.:. 0 R.:. 1)
                          , A.constant $ t R.! (R.Z R.:. 0 R.:. 2)
                          , A.constant $ t R.! (R.Z R.:. 1 R.:. 0)
                          , A.constant $ t R.! (R.Z R.:. 1 R.:. 1)
                          , A.constant $ t R.! (R.Z R.:. 1 R.:. 2)
                          , A.constant $ t R.! (R.Z R.:. 2 R.:. 0)
                          , A.constant $ t R.! (R.Z R.:. 2 R.:. 1)
                          , A.constant $ t R.! (R.Z R.:. 2 R.:. 2))
                    (t00, t01, t02, t10, t11, t12, _, _, _) = mat

                    i' = A.fromIntegral i
                    j' = A.fromIntegral j
                    it = i' * t00 + j' * t01 + t02
                    jt = i' * t10 + j' * t11 + t12
                in inShape (A.round jt) (A.round it) A.? (ch Channel.! A.index2 (A.round jt) (A.round it), 0)

rasterize :: (A.Elt a, A.IsFloating a, Image img (Channel2 a))
    => Transformed (img (Channel2 a)) -> img (Channel2 a)
rasterize (Transformed img t) = img & channels %~ Map.map (rasterizeChannel t)
