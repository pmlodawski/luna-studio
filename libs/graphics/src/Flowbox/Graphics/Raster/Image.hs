---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Graphics.Raster.Image (
    module Flowbox.Graphics.Raster.Image,
    Error(..)
) where

import           Control.Error
import           Data.Array.Accelerate             (Exp)
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.IO          as A
import qualified Data.Array.Repa                   as R
import qualified Data.Array.Repa.Algorithms.Matrix as M
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
--import qualified Debug.Trace                       as Dbg

import           Flowbox.Graphics.Raster.Channel (Channel, Channel2, Channel3, RawData2D, RawData3D)
import qualified Flowbox.Graphics.Raster.Channel as Channel
import           Flowbox.Graphics.Raster.Error   (Error (ChannelLookupError, TmpError))
import           Flowbox.Prelude                 hiding (lookup, map)


type String3 = (String, String, String)

type Stencil3x1 a       = (A.Stencil3 a, A.Stencil3 a, A.Stencil3 a)
type Stencil1x3 a       = (A.Stencil3 a, A.Stencil3 a, A.Stencil3 a)

data Transformation = Transformation (R.Array R.U R.DIM2 Double)
            deriving (Show)

data Image a = Image { _channels :: Map String (Channel a)
                     }
             deriving (Show, Eq, Ord)

data Transformed a = Transformed { src :: a
                                 , trans :: Transformation
                                 }
           deriving (Show)

instance Functor Transformed where
    --fmap :: (a -> b) -> Transformed a -> Transformed b
    fmap f (Transformed s t) = Transformed (f s) t

makeLenses ''Image



compute :: Channel.Backend ix a -> Image (A.Array ix a) -> Image (A.Array ix a)
compute backend img = Image $ Map.map (Channel.compute backend) $ view channels img


map :: (A.Shape ix, A.Elt a, A.Elt b) => (Exp a -> Exp b) -> Image (A.Array ix a) -> Image (A.Array ix b)
map f img = Image $ Map.map (Channel.map f) $ view channels img


lookup :: String -> Image (A.Array ix a) -> Either Error (Channel (A.Array ix a))
lookup name img = justErr (ChannelLookupError name) $ Map.lookup name (view channels img)


cpChannel :: String -> String -> Image (A.Array ix a) -> Either Error (Image (A.Array ix a))
cpChannel source destination img = do
    chan <- lookup source img
    return $ img & channels %~ (Map.insert destination chan)


insert :: String -> Channel (A.Array ix a) -> Image (A.Array ix a) -> Image (A.Array ix a)
insert name chan img = img & channels %~ (Map.insert name chan)

--reprFloat :: A.Shape ix => Image (A.Array ix A.Word8) -> Image (A.Array ix A.Float)
--reprFloat img = map (\c -> A.fromIntegral c / 255) img

--reprDouble :: A.Shape ix => Image (A.Array ix A.Word8) -> Image (A.Array ix A.Double)
--reprDouble img = map (\c -> A.fromIntegral c / 255) img

reprFloating :: (A.Shape ix, A.Elt a) => A.IsFloating a => Image (A.Array ix A.Word8) -> Image (A.Array ix a)
reprFloating img = map (\c -> A.fromIntegral c / 255) img

reprWord8 :: (A.Shape ix, A.Elt a, A.IsFloating a) => Image (A.Array ix a) -> Image (A.Array ix A.Word8)
reprWord8 img = map (\c -> A.truncate $ c * 255) img



-- transformations

transform :: a -> Transformed a
transform x = Transformed x mempty

rasterizeChannel :: (A.Elt a, A.IsFloating a) => Transformation -> Channel2 a -> Channel2 a
rasterizeChannel (Transformation t) ch =
    Channel.stencil (convolve3x1 kernel) A.Clamp $
    Channel.stencil (convolve1x3 kernel) A.Clamp $
    Channel.generate sh f
    where kernel = [1/6,2/3,1/6]
          convolve3x1 :: (A.Elt a, A.IsNum a) => [Exp a] -> Stencil3x1 a -> Exp a
          convolve3x1 kernel (_, (a,b,c), _) = sum $ zipWith (*) kernel [a,b,c]
          convolve1x3 :: (A.Elt a, A.IsNum a) => [Exp a] -> Stencil1x3 a -> Exp a
          convolve1x3 kernel ((_,a,_), (_,b,_), (_,c,_)) = sum $ zipWith (*) kernel [a,b,c]
          sh = Channel.shape ch
          (A.Z A.:. y A.:. x) = A.unlift sh
          inShape b a = ((a A.>=* 0) A.&&* (a A.<* x)) A.&&* ((b A.>=* 0) A.&&* (b A.<* y))
          f ix = let
                    (A.Z A.:. j A.:. i) = A.unlift ix
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

rasterize :: (A.Elt a, A.IsFloating a) => Transformed (Image (RawData2D a)) -> Image (RawData2D a)
rasterize (Transformed img t) = Image $ Map.map (rasterizeChannel t) $ view channels img


translate :: Double -> Double -> Transformed (Image a) -> Transformed (Image a)
translate x y (Transformed img transposition) = Transformed img transposition'
    where transposition' = mappend t transposition
          t = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [ 1, 0, -x
                                                                    , 0, 1, -y
                                                                    , 0, 0, 1 ])

rotate :: Double -> Transformed (Image a) -> Transformed (Image a)
rotate theta (Transformed img transposition) = Transformed img transposition'
    where transposition' = mappend t transposition
          t = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [   cos (-theta) , sin (-theta), 0
                                                                    , -(sin (-theta)), cos (-theta), 0
                                                                    , 0           , 0        , 1])

rotateAt :: Double -> Double -> Double -> Transformed (Image a) -> Transformed (Image a)
rotateAt theta x y = (translate (-x) (-y)) . (rotate theta) . (translate x y)

scale :: Double -> Double -> Transformed (Image a) -> Transformed (Image a)
scale x y (Transformed img transposition) = Transformed img transposition'
    where transposition' = mappend t transposition
          t = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [ 1/x, 0  , 0
                                                                    , 0  , 1/y, 0
                                                                    , 0  , 0  , 1])

scaleAt :: Double -> Double -> Double -> Double -> Transformed (Image a) -> Transformed (Image a)
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
