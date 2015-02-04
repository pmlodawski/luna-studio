---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module Flowbox.Graphics.Mockup (
      module Flowbox.Graphics.Mockup
    , module Math.Metric
    , module Math.Space.Space
    , A.Boundary(..)
    , variable
    , V2(..)
) where

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Type        as A
import           Data.Array.Accelerate.CUDA
import           Data.Bool
import           Math.Coordinate.Cartesian
import           Math.Space.Space
import           Math.Metric
import           Linear                            (V2(..))

import qualified Flowbox.Graphics.Color.Color                         as Color
import qualified Flowbox.Graphics.Color.Companding                    as Gamma
import           Flowbox.Graphics.Composition.Dither
import           Flowbox.Geom2D.Accelerate.CubicBezier
import           Flowbox.Geom2D.Accelerate.CubicBezier.Solve          as CubicSolveAcc
import           Flowbox.Geom2D.CubicBezier
import           Flowbox.Geom2D.Rectangle
import           Flowbox.Graphics.Composition.Generator.Gradient
import           Flowbox.Graphics.Shader.Matrix                       as Shader
import           Flowbox.Graphics.Shader.Pipe
import           Flowbox.Graphics.Shader.Rasterizer
import           Flowbox.Graphics.Composition.Generator.Shape
import           Flowbox.Graphics.Shader.Sampler                      as Shader
import           Flowbox.Graphics.Shader.Stencil                      as Stencil
import           Flowbox.Graphics.Shader.Shader                       as Shader
import           Flowbox.Graphics.Composition.Transform               as Transform
import           Flowbox.Graphics.Composition.Histogram
import qualified Flowbox.Graphics.Composition.Generator.Raster        as Raster
import           Flowbox.Graphics.Image.Channel                       as Channel
import           Flowbox.Graphics.Composition.Color
import           Flowbox.Graphics.Image.Image                         as Image
import           Flowbox.Graphics.Image.Error                         as Image
import           Flowbox.Graphics.Image.View                          as View
import           Flowbox.Graphics.Utils.Accelerate                    (variable)
import           Flowbox.Graphics.Utils.Utils
import           Flowbox.Math.Matrix                                  as M
import           Flowbox.Prelude                                      as P hiding (lookup)
import qualified Data.Array.Accelerate.CUDA                           as CUDA

import Luna.Target.HS (Pure (..), Safe (..), Value (..), autoLift, autoLift1, fromValue, val)
import Control.PolyApplicative ((<<*>>))

import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.ColorCorrect
import Flowbox.Graphics.Mockup.Matte


-- == COMPO



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
circularLuna = gradientLuna circularShape

conicalLuna :: Int -> Int -> Image
conicalLuna = gradientLuna conicalShape

squareLuna :: Int -> Image
squareLuna side = gradientLuna squareShape side side

diamondLuna :: Int -> Int -> Image
diamondLuna = gradientLuna diamondShape

radialShapeLuna :: (Metric a (Point2 (Exp Float)) (Exp Float), MetricCoord a Cartesian)
                => a -> Int -> Int -> Image
radialShapeLuna metric w h = gradientLuna (radialShape metric) w h

linearShapeLuna :: Int -> Int -> Image
linearShapeLuna = gradientLuna linearShape

gradientLuna :: forall e.
                      (A.Lift Exp e,
                       A.Plain e ~ Int) =>
                      Shader (Point2 (Exp Float)) (Exp Float) -> e -> e -> Image
gradientLuna gradient (variable -> width) (variable -> height) = channelToImageRGBA grad
    where grad = rasterizer $ monosampler $ gradientShader

          gradientShader = scale (Grid width height) $ Transform.translate (V2 0.5 0.5) $ mapper gray gradient
          gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]

          weightFun tickPos val1 weight1 val2 weight2 = mix tickPos val1 val2
          mapper = flip colorMapper weightFun
