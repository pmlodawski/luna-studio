---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

module Flowbox.Graphics.Mockup.Noise where

import qualified Data.Array.Accelerate as A
import           Math.Space.Space      (Grid (..))

import           Flowbox.Graphics.Composition.Generator.Noise.Billow
import           Flowbox.Graphics.Composition.Generator.Noise.Perlin
import qualified Flowbox.Graphics.Composition.Transform              as Transform
import           Flowbox.Graphics.Image.Image                        (Image)
import qualified Flowbox.Graphics.Shader.Rasterizer                  as Shader
import qualified Flowbox.Graphics.Shader.Sampler                     as Sampler
import           Flowbox.Graphics.Shader.Shader                      (CartesianShader)
import           Flowbox.Graphics.Utils.Accelerate                   (variable)
import           Flowbox.Prelude                                     as P hiding (lookup)

import Flowbox.Graphics.Mockup.Basic

perlinLuna :: Float -> Int -> Int -> Image
perlinLuna (variable -> z) = noiseLuna (perlinNoise z)

billowLuna :: Float -> Int -> Int -> Image
billowLuna (variable -> z) = noiseLuna (billowNoise z)

noiseLuna :: (A.IsFloating a, A.Elt a, A.Lift A.Exp e, A.Plain e ~ Int)
          => CartesianShader (A.Exp a) (A.Exp Float) -> e -> e -> Image
noiseLuna noise (variable -> width) (variable -> height) = channelToImageRGBA noise'
    where noise' = Shader.rasterizer $ Sampler.monosampler $ noiseShader

          noiseShader = Transform.scale (Grid width height) noise
