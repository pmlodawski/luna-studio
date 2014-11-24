---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE TypeOperators             #-}

module Flowbox.Geom2D.Rasterizer where

import           Data.Array.Accelerate (Array)
import           Data.Array.Accelerate.IO
import           Data.VectorSpace
import           Data.Word
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Segment
import           Diagrams.Prelude
import           Graphics.Rendering.Cairo hiding (translate)
import           System.IO.Unsafe

import           Math.Coordinate.Cartesian (Point2(..))
import           Flowbox.Graphics.Image.Image   (Image)
import           Flowbox.Graphics.Image.IO.BMP
import qualified Flowbox.Graphics.Image.View    as View
import           Flowbox.Math.Matrix
import           Flowbox.Prelude hiding ((#))



-- intended to be hidden from this package
f2d :: Real a => a -> Double
f2d = fromRational . toRational

makeSegments :: Real a => a -> [Point2 a] -> [Segment Closed R2]
makeSegments (f2d -> h) = combine
    where combine [] = []
          combine [_] = []
          combine (s':c1':c2':e':xs) = let
                  Point2 (f2d -> sx)  (f2d -> sy)  = s'
                  Point2 (f2d -> c1x) (f2d -> c1y) = c1'
                  Point2 (f2d -> c2x) (f2d -> c2y) = c1'
                  Point2 (f2d -> ex)  (f2d -> ey)  = e'
                  s  = r2 ( sx  , h - sy  )
                  c1 = r2 ( c1x , h - c1y )
                  c2 = r2 ( c2x , h - c2y )
                  e  = r2 ( ex  , h - ey  )
              in bezier3 (c1 ^-^ s) (c2 ^-^ s) (e ^-^ s) : combine (e':xs)
          combine _ = error "Flowbox.Geom2D.Rasterizer: unsupported ammount of points"

rasterizeVector :: Real a => DIM2 -> Bool -> [Point2 a] -> Image View.RGBA
rasterizeVector sh closed points = makeRGBA $ unsafePerformIO rasterize
    where Z :. w :. h = sh -- :: Z :. Int :. Int
          Point2 (f2d -> ox) (f2d -> oy) = head points
          rasterize = do
              let path = fromSegments $ makeSegments (fromIntegral h) points
                  diagram = case closed of
                      False -> path                        # translate (r2 (ox,oy)) # lc white # lw (Output 1)
                      True  -> (strokeLoop.closeLine) path # translate (r2 (ox,oy)) # fc white
                  (_, r) = renderDia Cairo (CairoOptions "" (Dims (fromIntegral w) (fromIntegral h)) RenderOnly True) (diagram :: Diagram Cairo R2)
              surface <- createImageSurface FormatARGB32 w h
              renderWith surface r
              bs <- imageSurfaceGetData surface
              fromByteString sh ((), bs)
