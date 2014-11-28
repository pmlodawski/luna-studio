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

import           Data.Array.Accelerate.IO
import           Data.Maybe
import           Data.VectorSpace
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Segment
import           Diagrams.Prelude
import           Graphics.Rendering.Cairo hiding (translate)
import           System.IO.Unsafe

import           Math.Coordinate.Cartesian (Point2(..))
import           Flowbox.Geom2D.ControlPoint
import           Flowbox.Graphics.Image.Image   (Image)
import           Flowbox.Graphics.Image.IO.BMP
import qualified Flowbox.Graphics.Image.View    as View
import           Flowbox.Math.Matrix
import           Flowbox.Prelude hiding ((#))



-- intended to be hidden from this package
f2d :: Real a => a -> Double
f2d = fromRational . toRational


makeSegments :: Real a => [ControlPoint a] -> [Segment Closed R2]
makeSegments points = combine points
    where combine []  = []
          combine [a'] = let
                  ControlPoint (Point2 ax ay) _ b' = f2d' a'
                  ControlPoint (Point2 dx dy) c' _ = f2d' $ head points
                  Point2 bx by = unpack b'
                  Point2 cx cy = unpack c'
                  a = r2 (ax , ay)
                  b = r2 (bx , by)
                  c = r2 (cx , cy)
                  d = r2 (dx , dy)
              in [bezier3 b (d ^+^ c ^-^ a) (d ^-^ a)]
          combine (a':d':xs) = let
                  ControlPoint (Point2 ax ay) _ b' = f2d' a'
                  ControlPoint (Point2 dx dy) c' _ = f2d' d'
                  Point2 bx by = unpack b'
                  Point2 cx cy = unpack c'
                  a = r2 (ax , ay)
                  b = r2 (bx , by)
                  c = r2 (cx , cy)
                  d = r2 (dx , dy)
              in bezier3 b (d ^+^ c ^-^ a) (d ^-^ a) : combine (d':xs)
          combine _ = error "Flowbox.Geom2D.Rasterizer: unsupported ammount of points"
          --step
          f2d'   = fmap f2d
          unpack = fromMaybe (Point2 0 0)

rasterizeVector :: Real a => Int -> Int -> Bool -> [ControlPoint a] -> Image View.RGBA
rasterizeVector w h closed points = makeRGBA $ unsafePerformIO rasterize
    where ControlPoint (Point2 ox oy) _ _ = fmap f2d $ head points
          h' = fromIntegral h
          rasterize = do
              let path = fromSegments $ makeSegments points
                  diagram = case closed of
                      False -> path                        # translate (r2 (ox,oy)) # scaleY (-1) # translateY h' # lc white # lw (Output 1)
                      True  -> (strokeLoop.closeLine) path # translate (r2 (ox,oy)) # scaleY (-1) # translateY h' # fc white # lw (Output 0)
                  (_, r) = renderDia Cairo (CairoOptions "" (Dims (fromIntegral w) (fromIntegral h)) RenderOnly True) (diagram :: Diagram Cairo R2)
              --print "MODULE:"
              --print diagram
              surface <- createImageSurface FormatARGB32 w h
              renderWith surface r
              bs <- imageSurfaceGetData surface
              fromByteString (Z:.w:.h) ((), bs)
