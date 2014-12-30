module Flowbox.Graphics.Image.Matte where

import Flowbox.Geom2D.Mask
import Flowbox.Geom2D.Shape
import Flowbox.Geom2D.Rasterizer
import Flowbox.Math.Matrix
import Flowbox.Prelude
import Data.Array.Accelerate as A

data Matte a = ImageMatte (Matrix2 a)
             | VectorMatte (Mask a)

rasterizeMatte :: (Real a, Fractional a) => DIM2 -> Matte Double -> Matrix2 Double
rasterizeMatte _ (ImageMatte mat) = mat
rasterizeMatte dim (VectorMatte mask) = rasterizeMask w h mask
	where
		Z:.h:.w = dim