---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE TypeOperators             #-}

module Flowbox.Geom2D.Rasterizer (
    module Flowbox.Geom2D.Rasterizer,
    Point2(..)
) where

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


makeSegments :: Real a => [Point2 a] -> [Segment Closed R2]
makeSegments = combine
    where combine [] = []
          combine [_] = []
          combine (a':b':c':d':xs) = let
                  Point2 (f2d -> ax) (f2d -> ay) = a'
                  Point2 (f2d -> bx) (f2d -> by) = b'
                  Point2 (f2d -> cx) (f2d -> cy) = c'
                  Point2 (f2d -> dx) (f2d -> dy) = d'
                  a = r2 ( ax , ay )
                  b = r2 ( bx , by )
                  c = r2 ( cx , cy )
                  d = r2 ( dx , dy )
                  --fix p = (r2 (0,h)) ^-^ (p ^-^ a)
                  fix p = p ^-^ a
              --in bezier3 (b ^-^ a) (c ^-^ a) (d ^-^ a) : combine (d':xs)
              in bezier3 (fix b) (fix c) (fix d) : combine (d':xs)
          combine _ = error "Flowbox.Geom2D.Rasterizer: unsupported ammount of points"

rasterizeVector :: Real a => Int -> Int -> Bool -> [Point2 a] -> Image View.RGBA
rasterizeVector w h closed points = makeRGBA $ unsafePerformIO rasterize
    where Point2 (f2d -> ox) (f2d -> oy) = head points
          h' = fromIntegral h
          rasterize = do
              let path = fromSegments $ makeSegments points
                  diagram = case closed of
                      False -> path                        # translate (r2 (ox,oy)) # scaleY (-1) # translateY h' # lc white # lw (Output 1)
                      True  -> (strokeLoop.closeLine) path # translate (r2 (ox,oy)) # scaleY (-1) # translateY h' # fc white
                  (_, r) = renderDia Cairo (CairoOptions "" (Dims (fromIntegral w) (fromIntegral h)) RenderOnly True) (diagram :: Diagram Cairo R2)
              --print "MODULE:"
              --print diagram
              surface <- createImageSurface FormatARGB32 w h
              renderWith surface r
              bs <- imageSurfaceGetData surface
              fromByteString (Z:.w:.h) ((), bs)
