---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module Flowbox.Graphics.Mockup.Transform (
    CropConstantOutside,
    CropReformat,
    Skew(..),
    SkewOrder(..),
    Transform(..),
    cropLuna,
    rotateAtLuna,
    scaleAtLuna,
    translateLuna,
    skewAtLuna,
    transformLuna
) where

import           Data.Array.Accelerate     (Exp, Z(..), (:.)(..))
import qualified Data.Array.Accelerate     as A
import           Linear                    (V2 (..))
import           Math.Coordinate.Cartesian (Point2 (..))
import           Math.Space.Space          (Grid (..))

import           Flowbox.Geom2D.Rectangle               (Rectangle)
import           Flowbox.Graphics.Composition.Transform (CropConstantOutside, CropReformat)
import qualified Flowbox.Graphics.Composition.Transform as Transform
import           Flowbox.Graphics.Image.Channel         (Channel (..), ChannelData (..))
import qualified Flowbox.Graphics.Image.Channel         as Channel
import           Flowbox.Graphics.Image.Matte           (Matte)
import qualified Flowbox.Graphics.Image.Matte           as Matte
import           Flowbox.Graphics.Image.Image           (Image)
import           Flowbox.Graphics.Shader.Shader         (Shader(..))
import qualified Flowbox.Graphics.Shader.Shader         as Shader
import           Flowbox.Graphics.Utils.Accelerate      (variable)
import qualified Flowbox.Math.Matrix                    as M
import           Flowbox.Prelude                        as P hiding (lookup)

import Flowbox.Graphics.Mockup.Basic


data SkewOrder = SkewXY | SkewYX

data Skew a = Skew { _skewValue :: V2 a
                   , _skewOrder :: SkewOrder
                   }

data Transform a = Transform { _translate :: V2 a
                             , _rotate    :: a
                             , _scale     :: V2 a
                             , _skew      :: Skew a
                             , _center    :: Point2 a
                             }

--cornerPinLuna :: Double -> Double
--              -> Double -> Double
--              -> Double -> Double
--              -> Double -> Double
--              -> Image
--              -> Image
--cornerPinLuna (variable -> p1x) (variable -> p1y)
--              (variable -> p2x) (variable -> p2y)
--              (variable -> p3x) (variable -> p3y)
--              (variable -> p4x) (variable -> p4y) img = img'
--    where img' = onEachChannel process img
--          process = rasterizer . monosampler . cornerPin (p1, p2, p3, p4) . nearest . fromMatrix (A.Constant 0)
--          p1 = Point2 p1x p1y
--          p2 = Point2 p2x p2y
--          p3 = Point2 p3x p3y
--          p4 = Point2 p4x p4y

cropLuna :: Rectangle (Exp Int) -> CropReformat -> Bool -> Image -> Image
cropLuna rect reformat constantOutside = onEachChannel cropChannel
    where cropChannel = \case
              ChannelFloat name zeData -> ChannelFloat name $ Transform.crop' rect reformat (if constantOutside then Just (0 :: Exp Float) else Nothing) zeData
              ChannelInt   name zeData -> ChannelInt   name $ Transform.crop' rect reformat (if constantOutside then Just (0 :: Exp Int)   else Nothing) zeData

--scaleToLuna :: A.Boundary (Exp Double) -> Int -> Int -> Image -> Image
--scaleToLuna boundary (variable -> x) (variable -> y) = onEachChannel $ rasterizer . monosampler . foo
--    where foo :: Matrix2 Double -> ContinuousShader (Exp Double)
--          foo = scale (Grid x y) . nearest . fromMatrix boundary

channelDim :: Channel -> (Int,Int)
channelDim = unpackAccDims . channelDimAcc

channelDimAcc :: Channel -> (Exp Int, Exp Int)
channelDimAcc (ChannelFloat _ (MatrixData mat)) = (h,w)
  where
    sh = M.shape mat
    Z :. h :. w = A.unlift sh :: Z :. (Exp Int) :. (Exp Int)

channelDimAcc (ChannelFloat _ (ContinuousData shader)) = (h,w)
  where
    Shader (Grid h w) _ = shader

channelDimAcc (ChannelFloat _ (DiscreteData shader)) = (h,w)
  where
    Shader (Grid h w) _ = shader


changeCoordinateSystem :: Channel -> Point2 (Exp Float) -> Point2 (Exp Float)
changeCoordinateSystem chan (Point2 x' y') = Point2 x' (h - y')
  where
    (h', _) = channelDimAcc chan
    h = A.fromIntegral h' :: Exp Float

processSkew :: Exp Float -> Exp Float -> SkewOrder -> Point2 (Exp Float) -> Point2 (Exp Float)
processSkew k k' order = case order of
                      SkewXY -> (Transform.verticalSkew k') . (Transform.horizontalSkew k)
                      SkewYX -> (Transform.horizontalSkew k) . (Transform.verticalSkew k')

strengthShader :: Maybe (Matte Float) -> Channel -> Point2 (Exp Float) -> Exp Float
strengthShader matte chan = case matte of
                              Nothing -> (\x -> 1)
                              Just m -> let (h,w) = channelDim chan
                                            Shader _ matteShader = Matte.matteToContinuous h w m
                                        in (\x -> (matteShader x))

rotateChannelAt :: Point2 Float -> Float -> Maybe (Matte Float) -> Channel -> Channel
rotateChannelAt (fmap variable -> (Point2 x y)) (variable -> phi) matte chan = (rotate chan)
    where
      vBefore = V2 (-x) (-y)
      vAfter  = V2 x y

      rotate = \case
          (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          (Channel.asContinuous -> ChannelInt name zeData) -> ChannelInt name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData

      strength = strengthShader matte chan

      transformation :: Point2 (Exp Float) -> Point2 (Exp Float)
      transformation pt = (changeCoordinateSystem chan) $ Transform.translate vBefore $ Transform.rotate ((-phi)*(strength pt)) $ Transform.translate vAfter $ (changeCoordinateSystem chan) pt

scaleChannelAt :: Point2 Float -> V2 Float -> Maybe (Matte Float) -> Channel -> Channel
scaleChannelAt (fmap variable -> (Point2 x y)) (fmap variable -> v) matte chan = (scale chan)
    where
      vBefore = V2 (-x) (-y)
      vAfter  = V2 x y

      scale = \case
        (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
        (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData

      strength = strengthShader matte chan

      transformation :: Point2 (Exp Float) -> Point2 (Exp Float)
      transformation pt = (changeCoordinateSystem chan) $ Transform.translate vBefore $ Transform.scale (fmap (* (strength pt)) v) $ Transform.translate vAfter $ (changeCoordinateSystem chan) pt

translateChannel :: V2 Float -> Maybe (Matte Float) -> Channel -> Channel
translateChannel (fmap variable -> V2 x y) matte chan = (translate chan)
  where
    t = V2 x (-y)

    translate = \case
        (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
        (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData

    strength = strengthShader matte chan

    transformation :: Point2 (Exp Float) -> Point2 (Exp Float)
    transformation pt = Transform.translate (fmap (*(strength pt)) t) pt

skewChannelAt :: Point2 Float -> Skew Float -> Maybe (Matte Float) -> Channel -> Channel
skewChannelAt (fmap variable -> p) (Skew (fmap variable -> V2 k k') order) matte chan = (skew chan)
  where
    Point2 x y = changeCoordinateSystem chan p
    vBefore = V2 (-x) (-y)
    vAfter  = V2 x y

    skew = \case
      (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
      (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData

    strength = strengthShader matte chan

    transformation :: Point2 (Exp Float) -> Point2 (Exp Float)
    transformation pt = Transform.translate vBefore $ (processSkew (str * k) (str * k') order) $ Transform.translate vAfter pt
      where
        str = strength pt

rotateAtLuna :: Point2 Float -> Float -> Maybe (Matte Float) -> Image -> Image
rotateAtLuna p ang matte = onEachChannel (rotateChannelAt p ang matte)

scaleAtLuna :: Point2 Float -> V2 Float -> Maybe (Matte Float) -> Image -> Image
scaleAtLuna p v matte = onEachChannel (scaleChannelAt p v matte)

translateLuna :: V2 Float -> Maybe (Matte Float) -> Image -> Image
translateLuna tr matte = onEachChannel (translateChannel tr matte)

skewAtLuna :: Point2 Float -> Skew Float -> Maybe (Matte Float) -> Image -> Image
skewAtLuna p skew matte = onEachChannel (skewChannelAt p skew matte)

transformLuna :: Transform Float -> Image -> Image
transformLuna tr = onEachChannel (transformChannel tr)
    where
      transformChannel :: Transform Float -> Channel -> Channel
      transformChannel (Transform tr phi sc skew ce) chan = transformation chan
        where
          transformation :: Channel -> Channel
          transformation = (translateChannel tr Nothing) . (rotateChannelAt ce phi Nothing) . (skewChannelAt ce skew Nothing) . (scaleChannelAt ce sc Nothing)
