---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module Flowbox.Geom2D.Rasterizer (
    module Flowbox.Geom2D.Rasterizer,
    Point2(..)
) where

import           Codec.Picture                   ( PixelRGBA8( .. ))
import qualified Codec.Picture                   as Juicy
import           Data.Array.Accelerate           ((&&*), (==*), (>*), (||*))
import qualified Data.Array.Accelerate           as A
import           Data.Array.Accelerate.IO
import           Data.Bits                       ((.&.))
import           Data.Maybe
import           Data.VectorSpace
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude                hiding (Path)
import           Diagrams.Segment
import qualified Graphics.Rasterific                            as Rasta
import qualified Graphics.Rasterific.Texture                    as RastaTex
import           Graphics.Rendering.Cairo        hiding (Path, translate)
import           System.IO.Unsafe

import           Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve
import           Flowbox.Geom2D.ControlPoint
import           Flowbox.Geom2D.CubicBezier
import           Flowbox.Geom2D.Mask
import           Flowbox.Geom2D.Path
import           Flowbox.Geom2D.QuadraticBezier
import           Flowbox.Geom2D.QuadraticBezier.Conversion
import qualified Flowbox.Graphics.Image.Channel                  as Channel
import           Flowbox.Graphics.Image.Image                    (Image)
import qualified Flowbox.Graphics.Image.Image                    as Image
import           Flowbox.Graphics.Image.IO.BMP
import qualified Flowbox.Graphics.Image.View                     as View
import qualified Flowbox.Graphics.Utils.Utils                    as U
import           Flowbox.Math.Matrix                             ((:.) (..), DIM2, Matrix (..), Matrix2, Z (..))
import qualified Flowbox.Math.Matrix                             as M
import           Flowbox.Prelude                                 hiding (use, ( # ))
import           Math.Coordinate.Cartesian                       (Point2 (..))



-- intended to be hidden from this package
f2d :: Real a => a -> Double
f2d = fromRational . toRational

f2f :: Real a => a -> Float
f2f = fromRational . toRational

-- TODO[1]: revert to this version when the wrapping model for handling GUI's use-case gets implemented
--unpackP :: Num a => Maybe (Point2 a) -> Point2 a
--unpackP = fromMaybe (Point2 0 0)
unpackP :: Fractional a => Point2 a -> Point2 a -> Maybe (Point2 a) -> Point2 a
unpackP a b = fromMaybe ((b - a)/3)


makeSegmentsNoDia :: (Real a, Fractional a) => Bool -> [ControlPoint a] -> [Rasta.CubicBezier]
makeSegmentsNoDia closed points = combine points
    where combine []   = []
          combine [a'] = if not closed then [] else let
                  ControlPoint pa@(Point2 ax ay) _ b' = f2f' a'
                  ControlPoint pd@(Point2 dx dy) c' _ = f2f' $ head points
                  Point2 bx by = unpackP pa pd b'
                  Point2 cx cy = unpackP pd pa c'
                  a = Rasta.V2 ax ay 
                  b = Rasta.V2 bx by
                  c = Rasta.V2 cx cy
                  d = Rasta.V2 dx dy
              in [Rasta.CubicBezier a b c d]
          combine (a':d':xs) = let
                  ControlPoint pa@(Point2 ax ay) _ b' = f2f' a'
                  ControlPoint pd@(Point2 dx dy) c' _ = f2f' d'
                  Point2 bx by = unpackP pa pd b'
                  Point2 cx cy = unpackP pd pa c'
                  a = Rasta.V2 ax ay
                  b = Rasta.V2 bx by
                  c = Rasta.V2 cx cy
                  d = Rasta.V2 dx dy
              in Rasta.CubicBezier a b c d : combine (d':xs)
          f2f' = fmap f2f

makeSegments :: (Real a, Fractional a) => Bool -> [ControlPoint a] -> [Segment Closed R2]
makeSegments closed points = combine points
    where combine []  = []
          combine [a'] = if not closed then [] else let
                  ControlPoint pa@(Point2 ax ay) _ b' = f2d' a'
                  ControlPoint pd@(Point2 dx dy) c' _ = f2d' $ head points
                  Point2 bx by = unpackP pa pd b'
                  Point2 cx cy = unpackP pd pa c'
                  a = r2 (ax , ay)
                  b = r2 (bx , by)
                  c = r2 (cx , cy)
                  d = r2 (dx , dy)
              in [bezier3 b (d ^+^ c ^-^ a) (d ^-^ a)]
          combine (a':d':xs) = let
                  ControlPoint pa@(Point2 ax ay) _ b' = f2d' a'
                  ControlPoint pd@(Point2 dx dy) c' _ = f2d' d'
                  Point2 bx by = unpackP pa pd b'
                  Point2 cx cy = unpackP pd pa c'
                  a = r2 (ax , ay)
                  b = r2 (bx , by)
                  c = r2 (cx , cy)
                  d = r2 (dx , dy)
              in bezier3 b (d ^+^ c ^-^ a) (d ^-^ a) : combine (d':xs)
          combine _ = error "Flowbox.Geom2D.Rasterizer.makeSegments: unsupported ammount of points"
          f2d' = fmap f2d

-- TODO[1]
--makeCubics :: Real a => Path a -> [CubicBezier a]
--makeCubics (Path closed points) = combine points
--    where combine [] = []
--          combine [a'] = if not closed then [] else let
--                  ControlPoint a _ (unpackP -> b) = a'
--                  ControlPoint d (unpackP -> c) _ = head points
--              in [CubicBezier a (a+b) (d+c) d]
--          combine (a':d':xs) = let
--                  ControlPoint a _ (unpackP -> b) = a'
--                  ControlPoint d (unpackP -> c) _ = d'
--              in CubicBezier a (a+b) (d+c) d : combine (d':xs)
--          combine _ = error "Flowbox.Geom2D.Rasterizer.makeCubics: unsupported ammount of points"
makeCubics :: (Real a, Fractional a) => Path a -> [CubicBezier a]
makeCubics (Path closed points) = combine points
    where combine [] = []
          combine [a'] = if not closed then [] else let
                  ControlPoint a _ b' = a'
                  ControlPoint d c' _ = head points
                  b = unpackP a d b'
                  c = unpackP d a c'
              in [CubicBezier a (a+b) (d+c) d]
          combine (a':d':xs) = let
                  ControlPoint a _ b' = a'
                  ControlPoint d c' _ = d'
                  b = unpackP a d b'
                  c = unpackP d a c'
              in CubicBezier a (a+b) (d+c) d : combine (d':xs)
          combine _ = error "Flowbox.Geom2D.Rasterizer.makeCubics: unsupported ammount of points"

pathToRGBA32NoDia :: (Real a, Fractional a) => Int -> Int -> Path a -> A.Array DIM2 RGBA32
pathToRGBA32NoDia w h (Path closed points) = imgToRGBA32 rasterize
    where rasterize = Rasta.renderDrawing w h (PixelRGBA8 0 0 0 255) $
                      Rasta.withTexture (RastaTex.uniformTexture (PixelRGBA8 255 255 255 255)) $ do
                          let cubics = makeSegmentsNoDia closed points
                          Rasta.stroke 4 Rasta.JoinRound (Rasta.CapRound, Rasta.CapRound) $ fmap Rasta.CubicBezierPrim cubics
          imgToRGBA32 (Juicy.Image width height vec) = error ""


pathToRGBA32 :: (Real a, Fractional a) => Int -> Int -> Path a -> A.Array DIM2 RGBA32
pathToRGBA32 w h (Path closed points) = unsafePerformIO rasterize
    where ControlPoint (Point2 ox oy) _ _ = fmap f2d $ head points
          h' = fromIntegral h
          rasterize = do
              let path = fromSegments $ makeSegments closed points
                  diagram = case closed of
                      False -> path                        # translate (r2 (ox,oy)) # scaleY (-1) # translateY h' # lc white # lw (Output 1)
                      True  -> (strokeLoop.closeLine) path # translate (r2 (ox,oy)) # scaleY (-1) # translateY h' # fc white # lw (Output 0)
                  (_, r) = renderDia Cairo (CairoOptions "" (Dims (fromIntegral w) (fromIntegral h)) RenderOnly True) (diagram :: Diagram Cairo R2)
              surface <- createImageSurface FormatARGB32 w h
              renderWith surface r
              bs <- imageSurfaceGetData surface
              fromByteString (Z:.h:.w) ((), bs)

pathToMatrix :: (Real a, Fractional a) => Int -> Int -> Path a -> Matrix2 Double
pathToMatrix w h path = extractArr $ pathToRGBA32 w h path
    where extractArr arr = Delayed $ A.map extractVal $ A.use arr
          extractVal :: M.Exp RGBA32 -> M.Exp Double
          extractVal rgba = (A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF) / 255

rasterizeMask :: (Real a, Fractional a) => Int -> Int -> Mask a -> Matrix2 Double
rasterizeMask w h (Mask path' feather') = path
    --case feather' of
    --    Nothing -> path
    --    Just feather' -> let
    --            feather = ptm feather'
    --            convert :: Real a => Path a -> A.Acc (A.Vector (QuadraticBezier Double))
    --            convert p = let
    --                    a = makeCubics p
    --                in A.use $ A.fromList (Z :. length a) $ convertCubicsToQuadratics 5 0.001 $ (fmap.fmap) f2d a
    --            cA = convert path'
    --            cB = convert feather'
    --        in M.generate (A.index2 (U.variable h) (U.variable w)) $ combine feather cA cB
    where ptm  = pathToMatrix w h
          path = ptm path'
          combine :: Matrix2 Double -> A.Acc (A.Vector (QuadraticBezier Double)) -> A.Acc (A.Vector (QuadraticBezier Double)) -> A.Exp A.DIM2 -> A.Exp Double
          combine feather pQ fQ idx@(A.unlift . A.unindex2 -> (A.fromIntegral -> y, A.fromIntegral -> x) :: (A.Exp Int, A.Exp Int)) = let
                  p  = path M.! idx
                  f  = feather M.! idx
                  d  = distanceFromQuadratics (A.lift $ Point2 x y)
                  dp = d pQ
                  df = d fQ
              in A.cond ((p >* 0 &&* f >* 0) ||* (p ==* 0 &&* f ==* 0)) p (dp / (dp+df) * p)

matrixToImage :: Matrix2 Double -> Image
matrixToImage a = Image.singleton view
    where view = View.append (Channel.ChannelFloat "rgba.r" $ Channel.MatrixData w)
               $ View.append (Channel.ChannelFloat "rgba.g" $ Channel.MatrixData w)
               $ View.append (Channel.ChannelFloat "rgba.b" $ Channel.MatrixData w)
               $ View.append (Channel.ChannelFloat "rgba.a" $ Channel.MatrixData a)
               $ View.empty "rgba"
          w = M.map (\_ -> 1) a
