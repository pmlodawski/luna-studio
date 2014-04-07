---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

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

import           Flowbox.Graphics.Image.Channel (ChannelAcc, Channel2, RawData2)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Error   (Error (ChannelLookupError), Result)
import           Flowbox.Graphics.Transform     (Transformation(..), Transformed(..))
import           Flowbox.Prelude                hiding (lookup, map)


data Image a = Image { _channels :: Map Channel.Name (Channel.Channel a)
                     }
             deriving (Show, Eq, Ord)

type ImageAcc ix a = Image (A.Array ix a)

type Stencil3x1 a = (A.Stencil3 a, A.Stencil3 a, A.Stencil3 a)
type Stencil1x3 a = (A.Stencil3 a, A.Stencil3 a, A.Stencil3 a)


makeLenses ''Image


-- ==== Computation

compute :: Channel.Backend ix a -> ImageAcc ix a -> ImageAcc ix a
compute backend img = Image $ Map.map (Channel.compute backend) $ view channels img

-- ==== Traversal

-- == Map

map :: (ChannelAcc ix a -> ChannelAcc ix b) -> ImageAcc ix a -> ImageAcc ix b
map f img = Image $ Map.map f $ view channels img

mapChannels :: (A.Shape ix, A.Elt a, A.Elt b) => (Exp a -> Exp b) -> ImageAcc ix a -> ImageAcc ix b
mapChannels f img = Image $ Map.map (Channel.map f) $ view channels img

mapWithKey :: (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b) -> ImageAcc ix a -> ImageAcc ix b
mapWithKey f img = Image $ Map.mapWithKey f $ view channels img

-- TODO: think about how to make this function work in regard to mapWithKey in a similar way map' works in reagard to map
--mapWithKey' :: (A.Shape ix, A.Elt a, A.Elt b) => (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b) -> ImageAcc ix a -> ImageAcc ix b
--mapWithKey' f img = Image $ Map.mapWithKey f $ view channels img

--mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccum :: (a -> ChannelAcc ix b -> (a, ChannelAcc ix c)) -> a -> ImageAcc ix b -> (a, ImageAcc ix c)
mapAccum f acc img = (fst result, Image $ snd result)
    where result = Map.mapAccum f acc (img ^. channels)

--mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey :: (a -> Channel.Name -> ChannelAcc ix b -> (a, ChannelAcc ix c)) -> a -> ImageAcc ix b -> (a, ImageAcc ix c)
mapAccumWithKey f acc img = (fst result, Image $ snd result)
    where result = Map.mapAccumWithKey f acc (img ^. channels)

-- == Folds

foldr :: (ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix b) -> ChannelAcc ix b -> ImageAcc ix a -> ChannelAcc ix b
foldr f acc img = Map.foldr f acc $ view channels img

foldl :: (ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix a) -> ChannelAcc ix a -> ImageAcc ix b -> ChannelAcc ix a
foldl f acc img = Map.foldl f acc $ view channels img

foldrWithKey :: (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix b) -> ChannelAcc ix b -> ImageAcc ix a -> ChannelAcc ix b
foldrWithKey f acc img = Map.foldrWithKey f acc $ view channels img

foldlWithKey :: (ChannelAcc ix a -> Channel.Name -> ChannelAcc ix b -> ChannelAcc ix a) -> ChannelAcc ix a -> ImageAcc ix b -> ChannelAcc ix a
foldlWithKey f acc img = Map.foldlWithKey f acc $ view channels img

-- ==== Accessors

-- == Getters

elementAt :: Int -> ImageAcc ix a -> (Channel.Name, ChannelAcc ix a)
elementAt pos img = Map.elemAt pos $ view channels img

get :: Channel.Name -> ImageAcc ix a -> Result (ChannelAcc ix a)
get cname img = justErr (ChannelLookupError cname) $ Map.lookup cname (view channels img)

-- == Setters

insert :: Channel.Name -> ChannelAcc ix a -> ImageAcc ix a -> ImageAcc ix a
insert cname chan img = img & channels %~ (Map.insert cname chan)

remove :: Channel.Name -> ImageAcc ix a -> ImageAcc ix a
remove cname img = img & channels %~ (Map.delete cname)

adjust :: (ChannelAcc ix a -> ChannelAcc ix a) -> String -> ImageAcc ix a -> ImageAcc ix a
adjust f cname img = img & channels %~ (Map.adjust f cname)

adjustWithKey :: (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix a) -> String -> ImageAcc ix a -> ImageAcc ix a
adjustWithKey f cname img = img & channels %~ (Map.adjustWithKey f cname)

update :: (ChannelAcc ix a -> Maybe (ChannelAcc ix a)) -> Channel.Name -> ImageAcc ix a -> ImageAcc ix a
update f cname img = img & channels %~ (Map.update f cname)

updateWithKey :: (Channel.Name -> ChannelAcc ix a -> Maybe (ChannelAcc ix a)) -> String -> ImageAcc ix a -> ImageAcc ix a
updateWithKey f cname img = img & channels %~ (Map.updateWithKey f cname)

alter :: (Maybe (ChannelAcc ix a) -> Maybe (ChannelAcc ix a)) -> Channel.Name -> ImageAcc ix a -> ImageAcc ix a
alter f cname img = img & channels %~ (Map.alter f cname)

cpChannel :: Channel.Name -> Channel.Name -> ImageAcc ix a -> Result (ImageAcc ix a)
cpChannel source destination img = do
    chan <- get source img
    return $ img & channels %~ (Map.insert destination chan)

-- ==== Combine

-- == Union

channelUnion :: ImageAcc ix a -> ImageAcc ix a -> ImageAcc ix a
channelUnion imgA imgB = Image $ Map.union (imgA ^. channels) (imgB ^. channels)

channelUnionWith :: (ChannelAcc ix a -> ChannelAcc ix a -> ChannelAcc ix a) -> ImageAcc ix a -> ImageAcc ix a -> ImageAcc ix a
channelUnionWith f imgA imgB = Image $ Map.unionWith f (imgA ^. channels) (imgB ^. channels)

channelUnionWithKey :: (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix a -> ChannelAcc ix a) -> ImageAcc ix a -> ImageAcc ix a -> ImageAcc ix a
channelUnionWithKey f imgA imgB = Image $ Map.unionWithKey f (imgA ^. channels) (imgB ^. channels)

-- == Intersection

channelIntersection :: ImageAcc ix a -> ImageAcc ix b -> ImageAcc ix a
channelIntersection imgA imgB = Image $ Map.intersection (imgA ^. channels) (imgB ^. channels)

channelIntersectionWith :: (ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix c) -> ImageAcc ix a -> ImageAcc ix b -> ImageAcc ix c
channelIntersectionWith f imgA imgB = Image $ Map.intersectionWith f (imgA ^. channels) (imgB ^. channels)

channelIntersectionWithKey :: (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix c) -> ImageAcc ix a -> ImageAcc ix b -> ImageAcc ix c
channelIntersectionWithKey f imgA imgB = Image $ Map.intersectionWithKey f (imgA ^. channels) (imgB ^. channels)

-- ==== Filter

-- TODO: filter, filterWithKey/filterWithName : naive implementation using Map.toAscList and Map.fromAscList
-- and implement filterByname using filterWithName
filterByName :: [Channel.Name] -> ImageAcc ix a -> ImageAcc ix a
filterByName names img = img & channels %~ (Map.filterWithKey nameMatches)
    where nameMatches cname _ = cname `elem` names

selectChannels :: Channel.Select -> ImageAcc ix a -> ImageAcc ix a
selectChannels channels img = case channels of
    Channel.AllChannels      -> img
    Channel.ChannelList list -> filterByName list img

-- ==== Conversion between numeric types

toFloat :: A.Shape ix => ImageAcc ix A.Word8 -> ImageAcc ix A.Float
toFloat img = mapChannels (\c -> A.fromIntegral c / 255) img

toDouble :: A.Shape ix => ImageAcc ix A.Word8 -> ImageAcc ix A.Double
toDouble img = mapChannels (\c -> A.fromIntegral c / 255) img

toFloating :: (A.Shape ix, A.Elt a) => A.IsFloating a => ImageAcc ix A.Word8 -> ImageAcc ix a
toFloating img = mapChannels (\c -> A.fromIntegral c / 255) img

toWord8 :: (A.Shape ix, A.Elt a, A.IsFloating a) => ImageAcc ix a -> ImageAcc ix A.Word8
toWord8 img = mapChannels (\c -> A.truncate $ c * 255) img



-- ==== TRANSFORMATIONS

-- FIXME: find a better way of resampling after rasterization
-- czysty kod kurwa
rasterizeChannel :: (A.Elt a, A.IsFloating a) => Transformation -> Channel2 a -> Channel2 a
rasterizeChannel (Transformation t) ch =
    Channel.stencil (convolve3x1 initialKernel) A.Clamp $
    Channel.stencil (convolve1x3 initialKernel) A.Clamp $
    Channel.generate sh f
    where initialKernel = [1/6,2/3,1/6]
          convolve3x1 :: (A.Elt a, A.IsNum a) => [Exp a] -> Stencil3x1 a -> Exp a
          convolve3x1 kernel (_, (a,b,c), _) = sum $ zipWith (*) kernel [a,b,c]
          convolve1x3 :: (A.Elt a, A.IsNum a) => [Exp a] -> Stencil1x3 a -> Exp a
          convolve1x3 kernel ((_,a,_), (_,b,_), (_,c,_)) = sum $ zipWith (*) kernel [a,b,c]
          sh = Channel.shape ch
          (A.Z A.:. y A.:. x) = A.unlift sh
          inShape b a = ((a A.>=* 0) A.&&* (a A.<* x)) A.&&* ((b A.>=* 0) A.&&* (b A.<* y))
          f idx = let
                    (A.Z A.:. j A.:. i) = A.unlift idx
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
                in
                    (inShape (A.round jt) (A.round it)) A.? (ch Channel.! (A.index2 (A.round jt) (A.round it)), 0)
                    --ch Channel.!! 0

rasterize :: (A.Elt a, A.IsFloating a) => Transformed (Image (RawData2 a)) -> Image (RawData2 a)
rasterize (Transformed img t) = Image $ Map.map (rasterizeChannel t) $ view channels img


--------------------------------------------------------------------------
---- INSTANCES
--------------------------------------------------------------------------

instance Monoid (Image a) where
    mempty        = Image mempty
    a `mappend` b = Image $ (view channels a) `mappend` (view channels b)
