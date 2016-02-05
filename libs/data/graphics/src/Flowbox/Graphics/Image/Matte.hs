module Flowbox.Graphics.Image.Matte where

import           Data.Array.Accelerate          as A
import           Flowbox.Geom2D.Mask
import           Flowbox.Geom2D.Rasterizer
import           Flowbox.Geom2D.Shape
import           Flowbox.Graphics.Image.Channel
import           Flowbox.Graphics.Shader.Shader
import           Flowbox.Math.Matrix
import           Flowbox.Prelude

data Matte a = ImageMatte Channel
             | VectorMatte (Mask a) deriving(Show)

imageMatteFloat :: Channel -> Matte Float
imageMatteFloat chan@(ChannelFloat _ _) = ImageMatte chan
imageMatteFloat _ = error "Wrong arguemnt type - int given, float expected."

imageMatteInt :: Channel -> Matte Int
imageMatteInt chan@(ChannelInt _ _) = ImageMatte chan
imageMatteInt _ = error "Wrong argument type - float given, int expected."

matteToMatrix :: Int -> Int -> Matte Float -> Matrix2 Float
matteToMatrix _ _ (ImageMatte d) = matte
  where (ChannelFloat _ (MatrixData matte)) = asMatrix d

matteToMatrix h w (VectorMatte d) = matte
  where
    matte = rasterizeMask w h d

matteToDiscrete :: Int -> Int -> Matte Float -> DiscreteShader (A.Exp Float)
matteToDiscrete _ _ (ImageMatte d) = matte
  where (ChannelFloat _ (DiscreteData matte)) = asDiscrete d

matteToDiscrete h w m@(VectorMatte d) = matte
  where DiscreteData matte = asDiscreteData (A.constant 0) (MatrixData (matteToMatrix h w m))

matteToContinuous :: Int -> Int -> Matte Float -> ContinuousShader (A.Exp Float)
matteToContinuous _ _ (ImageMatte d) = matte
  where (ChannelFloat _ (ContinuousData matte)) = asContinuous d

matteToContinuous h w m@(VectorMatte d) = matte
  where (ContinuousData matte) = asContinuousData (A.constant 0) (MatrixData (matteToMatrix h w m))
