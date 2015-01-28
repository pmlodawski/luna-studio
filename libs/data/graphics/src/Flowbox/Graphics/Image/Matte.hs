module Flowbox.Graphics.Image.Matte where

import Flowbox.Geom2D.Mask
import Flowbox.Geom2D.Shape
import Flowbox.Geom2D.Rasterizer
import Flowbox.Math.Matrix
import Flowbox.Prelude
import Data.Array.Accelerate as A
import Flowbox.Graphics.Image.Channel
import Flowbox.Graphics.Shader.Shader

data Matte a = ImageMatte Channel
             | VectorMatte (Mask a) deriving(Show)

imageMatteDouble :: Channel -> Matte Double
imageMatteDouble chan@(ChannelFloat _ _) = ImageMatte chan
imageMatteDouble _ = error "Wrong arguemnt type - int given, float expected."

imageMatteInt :: Channel -> Matte Int
imageMatteInt chan@(ChannelInt _ _) = ImageMatte chan
imageMatteInt _ = error "Wrong argument type - float given, int expected."

matteToMatrix :: Int -> Int -> Matte Double -> Matrix2 Double
matteToMatrix _ _ (ImageMatte d) = matte
  where (ChannelFloat _ (MatrixData matte)) = asMatrix d

matteToMatrix h w (VectorMatte d) = matte
  where 
    matte = rasterizeMask w h d

matteToDiscrete :: Int -> Int -> Matte Double -> DiscreteShader (A.Exp Double)
matteToDiscrete _ _ (ImageMatte d) = matte
  where (ChannelFloat _ (DiscreteData matte)) = asDiscrete d

matteToDiscreteShader h w m@(VectorMatte d) = matte
  where DiscreteData matte = asDiscreteData (A.constant 0) (MatrixData (matteToMatrix h w m))

matteToContinuous :: Int -> Int -> Matte Double -> ContinuousShader (A.Exp Double)
matteToContinuous _ _ (ImageMatte d) = matte
  where (ChannelFloat _ (ContinuousData matte)) = asContinuous d

matteToContinuous h w m@(VectorMatte d) = matte
  where (ContinuousData matte) = asContinuousData (A.constant 0) (MatrixData (matteToMatrix h w m))