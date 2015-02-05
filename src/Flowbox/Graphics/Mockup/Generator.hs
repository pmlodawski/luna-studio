---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

module Flowbox.Graphics.Mockup.Generator (
    circularLuna,
    conicalLuna,
    constantLuna,
    diamondLuna,
    gradientLuna,
    linearShapeLuna,
    radialShapeLuna,
    squareLuna,
) where

import qualified Data.Array.Accelerate as A

import Linear                    (V2 (..))
import Math.Coordinate           (Cartesian)
import Math.Coordinate.Cartesian (Point2 (..))
import Math.Metric               (Metric, MetricCoord)
import Math.Space.Space          (Grid (..))

import qualified Flowbox.Graphics.Color.Color                    as Color
import           Flowbox.Graphics.Composition.Generator.Gradient (Tick (..))
import qualified Flowbox.Graphics.Composition.Generator.Gradient as Gradient
import qualified Flowbox.Graphics.Composition.Generator.Raster   as Raster
import qualified Flowbox.Graphics.Composition.Transform          as Transform
import           Flowbox.Graphics.Image.Image                    (Image)
import qualified Flowbox.Graphics.Shader.Rasterizer              as Shader
import qualified Flowbox.Graphics.Shader.Sampler                 as Sampler
import           Flowbox.Graphics.Shader.Shader                  (Shader (..))
import           Flowbox.Graphics.Utils.Accelerate               (variable)
import qualified Flowbox.Graphics.Utils.Utils                    as U
import           Flowbox.Prelude                                 as P hiding (lookup)

import Flowbox.Graphics.Mockup.Basic

constantLuna :: Int -> Int -> Color.RGBA Float -> Image
constantLuna (variable -> width) (variable -> height) (fmap variable -> Color.RGBA r g b a) =
    Raster.constant (A.index2 height width) chans
    where chans = [ ("rgba.r", r)
                  , ("rgba.g", g)
                  , ("rgba.b", b)
                  , ("rgba.a", a)
                  ]

--TODO[KM]: port checkerboard to luna
--type CheckerboardColorsLuna = (VPS ColorD, VPS ColorD, VPS ColorD, VPS ColorD)
--type CheckerboardLineLuna   = (VPS ColorD, VPS Double)
-- ...
--checkerboardLuna :: VPS Int -> VPS Int -> Double -> CheckerboardColorsLuna -> CheckerboardLineLuna -> CheckerboardLineLuna -> Image
--checkerboardLuna w h

circularLuna :: Int -> Int -> Image
circularLuna = gradientLuna Gradient.circularShape

conicalLuna :: Int -> Int -> Image
conicalLuna = gradientLuna Gradient.conicalShape

squareLuna :: Int -> Image
squareLuna side = gradientLuna Gradient.squareShape side side

diamondLuna :: Int -> Int -> Image
diamondLuna = gradientLuna Gradient.diamondShape

radialShapeLuna :: (Metric a (Point2 (A.Exp Float)) (A.Exp Float), MetricCoord a Cartesian)
                => a -> Int -> Int -> Image
radialShapeLuna metric w h = gradientLuna (Gradient.radialShape metric) w h

linearShapeLuna :: Int -> Int -> Image
linearShapeLuna = gradientLuna Gradient.linearShape

gradientLuna :: (A.Lift A.Exp e, A.Plain e ~ Int)
             => Shader (Point2 (A.Exp Float)) (A.Exp Float) -> e -> e -> Image
gradientLuna gradient (variable -> width) (variable -> height) = channelToImageRGBA grad
    where grad = Shader.rasterizer $ Sampler.monosampler $ gradientShader

          gradientShader = Transform.scale (Grid width height) $ Transform.translate (V2 0.5 0.5) $ mapper gray gradient
          gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]

          weightFun tickPos val1 _ val2 _ = U.mix tickPos val1 val2
          mapper = flip Gradient.colorMapper weightFun
