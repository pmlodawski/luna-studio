---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Mockup.Transform where

import Data.Array.Accelerate     (Exp)
import Linear                    (V2 (..))
import Math.Coordinate.Cartesian (Point2 (..))

import           Flowbox.Geom2D.Rectangle               (Rectangle)
import qualified Flowbox.Graphics.Composition.Transform as Transform
import           Flowbox.Graphics.Image.Channel         (Channel (..), ChannelData (..))
import qualified Flowbox.Graphics.Image.Channel         as Channel
import           Flowbox.Graphics.Image.Image           (Image)
import qualified Flowbox.Graphics.Shader.Shader         as Shader
import           Flowbox.Graphics.Utils.Accelerate      (variable)
import           Flowbox.Prelude                        as P hiding (lookup)

import Flowbox.Graphics.Mockup.Basic



data SkewOrder = SkewXY | SkewYX

data Skew a = Skew { _skewPoint :: V2 a
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

cropLuna :: Rectangle Int -> Image -> Image
cropLuna rect = onEachChannel cropChannel
    where cropChannel = \case
              ChannelFloat name zeData -> ChannelFloat name $ Transform.crop rect zeData
              ChannelInt   name zeData -> ChannelInt   name $ Transform.crop rect zeData

translateLuna :: V2 Float -> Image -> Image
translateLuna (fmap variable -> V2 x y) = onEachChannel translateChannel
    where v    = V2 x (-y)
          mask = Nothing
          translateChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          transformation :: Point2 (Exp Float) -> Point2 (Exp Float)
          transformation pt = Transform.translate (strength pt) pt
          strength :: Point2 (Exp Float) -> V2 (Exp Float)
          strength pt = case mask of
              Nothing      -> v
              --TODO[KM]: handle the mask properly (aka. get rid of that ugly pattern match) and uncomment the other case option
              --          and keep in mind that applying mask for functions using a center point might cause the shader to extract the strength value from wrong mask's coordinates
              _ -> v
              --Just (VPS m) -> let
              --        Right rgba = Image.lookupPrimary m
              --        unpackMat (Right (Just (ChannelFloat _ (asMatrixData -> MatrixData c)))) = c -- TODO[KM]: this ugly pattern match :D
              --        m' = unpackMat $ View.get rgba "rgba.r"
              --        Shader _ str = Shader.nearest $ Shader.fromMatrix (A.Constant (0 :: Exp Double)) $ m'
              --        mult :: Point2 (Exp Double) -> Exp Double -> Exp Double
              --        mult pt x = str pt * x
              --    in (fmap (mult pt) v)

rotateLuna :: Float -> Image -> Image
rotateLuna (variable -> phi) = onEachChannel rotateChannel
    where mask = Nothing
          rotateChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          transformation :: Point2 (Exp Float) -> Point2 (Exp Float)
          transformation pt = Transform.rotate (strength pt) pt
          strength :: Point2 (Exp Float) -> Exp Float
          strength pt = case mask of
              Nothing -> phi
              --TODO[KM]: handle the mask properly
              _       -> phi

rotateAtLuna :: Point2 Float -> Float -> Image -> Image
rotateAtLuna (fmap variable -> (Point2 x y)) (variable -> phi) = onEachChannel rotateChannel
    where vBefore = V2 x y
          vAfter  = V2 (-x) (-y)
          mask    = Nothing
          rotateChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          transformation :: Point2 (Exp Float) -> Point2 (Exp Float)
          transformation pt = Transform.translate vAfter $ Transform.rotate (strength pt) $ Transform.translate vBefore pt
          strength :: Point2 (Exp Float) -> Exp Float
          strength pt = case mask of
              Nothing -> phi
              --TODO[KM]: handle the mask properly
              _       -> phi

scaleLuna :: V2 Float -> Image -> Image
scaleLuna (fmap variable -> v) = onEachChannel scaleChannel
    where mask = Nothing
          scaleChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          transformation :: Point2 (Exp Float) -> Point2 (Exp Float)
          transformation pt = Transform.scale (strength pt) pt
          strength :: Point2 (Exp Float) -> V2 (Exp Float)
          strength pt = case mask of
              Nothing -> v
              --TODO[KM]: handle the mask properly
              _       -> v

scaleAtLuna :: Point2 Float -> V2 Float -> Image -> Image
scaleAtLuna (fmap variable -> (Point2 x y)) (fmap variable -> v) = onEachChannel scaleChannel
    where vBefore = V2 (-x) y
          vAfter  = V2 x (-y)
          mask    = Nothing
          scaleChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          transformation :: Point2 (Exp Float) -> Point2 (Exp Float)
          transformation pt = Transform.translate vAfter $ Transform.scale (strength pt) $ Transform.translate vBefore pt
          strength :: Point2 (Exp Float) -> V2 (Exp Float)
          strength pt = case mask of
              Nothing -> v
              --TODO[KM]: handle the mask properly
              _       -> v

--scaleToLuna :: A.Boundary (A.Exp Double) -> Int -> Int -> Image -> Image
--scaleToLuna boundary (variable -> x) (variable -> y) = onEachChannel $ rasterizer . monosampler . foo
--    where foo :: Matrix2 Double -> ContinuousShader (A.Exp Double)
--          foo = scale (Grid x y) . nearest . fromMatrix boundary

transformLuna :: Transform Float -> Image -> Image
transformLuna _ img = img
--transformLuna (Transform tr (variable -> phi) (fmap variable -> sc) _ ce) = onEachChannel transformChannel
--    where V2     translateX translateY = fmap variable tr
--          Point2 centerX    centerY    = fmap variable ce
--          vBefore = V2 (-centerX) centerY
--          vAfter  = V2 centerX (-centerY)
--          mask    = Nothing
--          tr      = V2 translateX (-translateY)
--          transformChannel = \case
--              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
--              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
--          transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
--          transformation pt = Transform.translate (strengthTranslate pt) $ Transform.translate vAfter $ Transform.rotate (strengthRotate pt) $ Transform.scale (strengthScale pt) $ Transform.translate vBefore pt
--          strengthRotate    = strengthScalar phi
--          strengthScale     = strengthVector sc
--          strengthTranslate = strengthVector tr
--          -- TODO: extract those 2 functions as a top-level binding
--          strengthScalar :: Exp Double -> Point2 (Exp Double) -> Exp Double
--          strengthScalar val pt = case mask of
--              Nothing -> val
--              --TODO[KM]: handle the mask properly
--              _       -> val
--          strengthVector :: V2 (Exp Double) -> Point2 (Exp Double) -> V2 (Exp Double)
--          strengthVector v pt = case mask of
--              Nothing -> v
--              --TODO[KM]: handle the mask properly
--              _       -> v
