module Flowbox.Graphics.Image.Matte where

import Flowbox.Geom2D.Mask
import Flowbox.Geom2D.Shape
import Flowbox.Geom2D.Rasterizer
import Flowbox.Math.Matrix
import Flowbox.Prelude
import Data.Array.Accelerate as A
import Flowbox.Graphics.Shader.Matrix as M
import Flowbox.Graphics.Shader.Shader as S
import Flowbox.Graphics.Shader.Sampler
import Flowbox.Graphics.Image.Channel

data Matte = ImageMatte Channel
           | VectorMatte (Mask Double)

--rasterizeMatte :: DIM2 -> Matte Double -> Matrix2 Double
--rasterizeMatte _ (ImageMatte matte) = matte
--rasterizeMatte dim (VectorMatte mask) = rasterizeMask w h mask
--	where
--		Z:.h:.w = dim

--matteAsDiscreteShader :: DIM2 -> Matte Double -> S.DiscreteShader (A.Exp Double)
--matteAsDiscreteShader dim matte = M.fromMatrix (A.Constant 0.0) (rasterizeMatte dim matte)

--matteAsContinuousShader :: DIM2 -> Matte Double -> S.ContinuousShader (A.Exp Double)
--matteAsContinuousShader dim matte = nearest (matteAsDiscreteShader dim matte)
