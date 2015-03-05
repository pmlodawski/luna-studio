---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

module Flowbox.Graphics.Mockup.Noise (
    noiseLuna,
    Quality(..),
    NoiseParams(..)
) where

import qualified Data.Array.Accelerate as A
import           Math.Space.Space      (Grid (..))

import           Flowbox.Graphics.Composition.Generator.Noise.Billow
import           Flowbox.Graphics.Composition.Generator.Noise.Internal    (Quality(..))
import           Flowbox.Graphics.Composition.Generator.Noise.Perlin
import           Flowbox.Graphics.Composition.Generator.Noise.RidgedMulti
import qualified Flowbox.Graphics.Composition.Transform                   as Transform
import           Flowbox.Graphics.Image.Image                             (Image)
import qualified Flowbox.Graphics.Shader.Rasterizer                       as Shader
import qualified Flowbox.Graphics.Shader.Sampler                          as Sampler
import           Flowbox.Graphics.Shader.Shader                           (CartesianShader)
import           Flowbox.Graphics.Utils.Accelerate                        (variable)
import           Flowbox.Prelude                                          as P hiding (lookup)

import Flowbox.Graphics.Mockup.Basic



data NoiseParams = Perlin { quality     :: Quality
                          , frequency   :: Float
                          , lacunarity  :: Float
                          , octaves     :: Int
                          , persistence :: Float
                          , seed        :: Int
                          , z           :: Float
                          }
                 | Billow { quality     :: Quality
                          , frequency   :: Float
                          , lacunarity  :: Float
                          , octaves     :: Int
                          , persistence :: Float
                          , seed        :: Int
                          , z           :: Float
                          }
                 | RidgedMulti { quality    :: Quality
                               , frequency  :: Float
                               , lacunarity :: Float
                               , octaves    :: Int
                               , maxOctave  :: Int
                              -- , seed       :: Int
                               , exponent   :: Float
                               , offset     :: Float
                               , gain       :: Float
                               , z          :: Float
                               }

noiseLuna :: NoiseParams -> Int -> Int -> Image
noiseLuna noiseParams (variable -> width) (variable -> height) = channelToImageRGBA noise'
    where noise' = Shader.rasterizer $ Sampler.monosampler $ noiseShader

          noiseShader = Transform.scale (Grid width height) $ case noiseParams of
              Perlin q f l o p s z -> perlinGen q (variable f) (variable l) (variable o) (variable p) (variable s) (variable z)
              Billow q f l o p s z -> billowGen q (variable f) (variable l) (variable o) (variable p) (variable s) (variable z)
              RidgedMulti q f l oc m e o g z -> ridgedMultiGen q (variable f) (variable l) (variable oc) (variable m) 1 (variable e) (variable o) (variable g) (variable z)
