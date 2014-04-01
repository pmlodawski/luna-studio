---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Graphics.Image (
    module Flowbox.Graphics.Image,
    Error(..)
) where

import           Control.Error
import           Data.Array.Accelerate             (Exp)
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Repa                   as R
import qualified Data.Array.Repa.Algorithms.Matrix as M
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
--import qualified Debug.Trace                       as Dbg

import           Flowbox.Graphics.Image.Channel (ChannelAcc, Channel2, RawData2)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Error   (Error (ChannelLookupError))
import           Flowbox.Prelude                 hiding (lookup, map)


data Image a = Image { _channels :: Map Channel.Name (Channel.Channel a)
                     }
             deriving (Show, Eq, Ord)

type ImageAcc ix a = Image (A.Array ix a)

type Stencil3x1 a       = (A.Stencil3 a, A.Stencil3 a, A.Stencil3 a)
type Stencil1x3 a       = (A.Stencil3 a, A.Stencil3 a, A.Stencil3 a)

-- FIXME [km]: move to a new module "Transformation" vvv

type RepaMatrix2 a = R.Array R.U R.DIM2 a
data Transformation = Transformation (RepaMatrix2 Double)
            deriving (Show)

data Transformed a = Transformed { src   :: a
                                 , trans :: Transformation
                                 }
           deriving (Show)

instance Functor Transformed where
    --fmap :: (a -> b) -> Transformed a -> Transformed b
    fmap f (Transformed s t) = Transformed (f s) t

-- FIXME: move to module Composition vvv
data Premultiply = Premultiply {name :: String, invert :: Bool}
                 | Unpremultiply {name:: String, invert :: Bool}

-- TODO: finish this data type
data Mask = Mask

makeLenses ''Image



compute :: Channel.Backend ix a -> ImageAcc ix a -> ImageAcc ix a
compute backend img = Image $ Map.map (Channel.compute backend) $ view channels img


map :: (ChannelAcc ix a -> ChannelAcc ix b) -> ImageAcc ix a -> ImageAcc ix b
map f img = Image $ Map.map f $ view channels img

-- FIXME: rename to sth, ie. mapByElement
map' :: (A.Shape ix, A.Elt a, A.Elt b) => (Exp a -> Exp b) -> ImageAcc ix a -> ImageAcc ix b
map' f img = Image $ Map.map (Channel.map f) $ view channels img

mapWithKey :: (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b) -> ImageAcc ix a -> ImageAcc ix b
mapWithKey f img = Image $ Map.mapWithKey f $ view channels img

-- TODO: think about how to make this function work in regard to mapWithKey in a similar way map' works in reagard to map
--mapWithKey' :: (A.Shape ix, A.Elt a, A.Elt b) => (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b) -> ImageAcc ix a -> ImageAcc ix b
--mapWithKey' f img = Image $ Map.mapWithKey f $ view channels img

foldr :: (ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix b) -> ChannelAcc ix b -> ImageAcc ix a -> ChannelAcc ix b
foldr f acc img = Map.foldr f acc $ view channels img

foldl :: (ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix a) -> ChannelAcc ix a -> ImageAcc ix b -> ChannelAcc ix a
foldl f acc img = Map.foldl f acc $ view channels img

foldrWithKey :: (Channel.Name -> ChannelAcc ix a -> ChannelAcc ix b -> ChannelAcc ix b) -> ChannelAcc ix b -> ImageAcc ix a -> ChannelAcc ix b
foldrWithKey f acc img = Map.foldrWithKey f acc $ view channels img

foldlWithKey :: (ChannelAcc ix a -> Channel.Name -> ChannelAcc ix b -> ChannelAcc ix a) -> ChannelAcc ix a -> ImageAcc ix b -> ChannelAcc ix a
foldlWithKey f acc img = Map.foldlWithKey f acc $ view channels img

-- handling channels

elementAt :: Int -> ImageAcc ix a -> (Channel.Name, ChannelAcc ix a)
elementAt pos img = Map.elemAt pos $ view channels img

insert :: Channel.Name -> ChannelAcc ix a -> ImageAcc ix a -> ImageAcc ix a
insert cname chan img = img & channels %~ (Map.insert cname chan)

get :: Channel.Name -> ImageAcc ix a -> Either Error (ChannelAcc ix a)
get cname img = justErr (ChannelLookupError cname) $ Map.lookup cname (view channels img)

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

cpChannel :: Channel.Name -> Channel.Name -> ImageAcc ix a -> Either Error (ImageAcc ix a)
cpChannel source destination img = do
    chan <- get source img
    return $ img & channels %~ (Map.insert destination chan)

-- TODO: filter, filterWithKey/filterWithName : naive implementation using Map.toAscList and Map.fromAscList
-- and implement filterByname using filterWithName
filterByName :: [Channel.Name] -> ImageAcc ix a -> ImageAcc ix a
filterByName names img = img & channels %~ (Map.filterWithKey nameMatches)
    where nameMatches cname _ = cname `elem` names

-- conversion between numeric types

toFloat :: A.Shape ix => ImageAcc ix A.Word8 -> ImageAcc ix A.Float
toFloat img = map' (\c -> A.fromIntegral c / 255) img

toDouble :: A.Shape ix => ImageAcc ix A.Word8 -> ImageAcc ix A.Double
toDouble img = map' (\c -> A.fromIntegral c / 255) img

toFloating :: (A.Shape ix, A.Elt a) => A.IsFloating a => ImageAcc ix A.Word8 -> ImageAcc ix a
toFloating img = map' (\c -> A.fromIntegral c / 255) img

toWord8 :: (A.Shape ix, A.Elt a, A.IsFloating a) => ImageAcc ix a -> ImageAcc ix A.Word8
toWord8 img = map' (\c -> A.truncate $ c * 255) img



-- TRANSFORMATIONS
transform :: a -> Transformed a
transform x = Transformed x mempty

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

translate :: Double -> Double -> Transformed a -> Transformed a
translate x y (Transformed img transposition) = Transformed img transposition'
    where transposition' = mappend t transposition
          t = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [ 1, 0, -x
                                                                    , 0, 1, -y
                                                                    , 0, 0, 1 ])

rotate :: Double -> Transformed a -> Transformed a
rotate theta (Transformed img transposition) = Transformed img transposition'
    where transposition' = mappend t transposition
          t = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [   cos (-theta) , sin (-theta), 0
                                                                    , -(sin (-theta)), cos (-theta), 0
                                                                    , 0           , 0        , 1])

rotateAt :: Double -> Double -> Double -> Transformed a -> Transformed a
rotateAt theta x y = (translate (-x) (-y)) . (rotate theta) . (translate x y)

scale :: Double -> Double -> Transformed a -> Transformed a
scale x y (Transformed img transposition) = Transformed img transposition'
    where transposition' = mappend t transposition
          t = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [ 1/x, 0  , 0
                                                                    , 0  , 1/y, 0
                                                                    , 0  , 0  , 1])

scaleAt :: Double -> Double -> Double -> Double -> Transformed a -> Transformed a
scaleAt sx sy x y = (translate (-x) (-y)) . (scale sx sy) . (translate x y)


--------------------------------------------------------------------------
---- INSTANCES
--------------------------------------------------------------------------

instance Monoid (Image a) where
    mempty        = Image mempty
    a `mappend` b = Image $ (view channels a) `mappend` (view channels b)

instance Monoid Transformation where
    mempty = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [ 1, 0, 0
                                                                   , 0, 1, 0
                                                                   , 0, 0, 1])
    (Transformation a) `mappend` (Transformation b) = Transformation (M.mmultS a b)
