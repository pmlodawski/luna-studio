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

import           Codec.Picture                                  ( PixelRGBA8( .. ))
import qualified Codec.Picture                                  as Juicy
import qualified Codec.Picture.Types                            as JuicyTypes
import           Control.Parallel.Strategies
import           Data.Array.Accelerate                          ((&&*), (==*), (>*), (||*))
import qualified Data.Array.Accelerate                          as A
import           Data.Array.Accelerate.IO
import           Data.Bits                                      ((.&.))
import           Data.Maybe
import           Data.VectorSpace
import           Data.Vector.Storable                           ( unsafeCast )
import qualified Data.Vector.Storable                           as S
import           Debug.Trace


import qualified Graphics.Rasterific                            as Rasta
import qualified Graphics.Rasterific.Texture                    as RastaTex
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
import qualified Flowbox.Prelude                                 as P
import           Math.Coordinate.Cartesian                       (Point2 (..))



f2d :: Real a => a -> Double
f2d = fromRational . toRational

f2f :: Real a => a -> Float
f2f = fromRational . toRational

-- TODO[1]: revert to this version when the wrapping model for handling GUI's use-case gets implemented
--unpackP :: Num a => Maybe (Point2 a) -> Point2 a
--unpackP = fromMaybe (Point2 0 0)
--unpackP :: Fractional a => Point2 a -> Point2 a -> Maybe (Point2 a) -> Point2 a
--unpackP a b = fromMaybe ((b - a)/3)

unpackP :: Fractional a => Point2 a -> Point2 a -> Maybe(Point2 a) -> Point2 a
unpackP a b c = fromMaybeNoDia ((b - a)/3 + a) a c

fromMaybeNoDia :: Fractional a => Point2 a -> Point2 a -> Maybe(Point2 a) -> Point2 a
fromMaybeNoDia def _ Nothing = def
fromMaybeNoDia _ a (Just b)  = a + b

makeSegments :: (Real a, Fractional a) => Bool -> [ControlPoint a] -> [Rasta.CubicBezier]
makeSegments closed points = combine points
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
          combine _ = error "Flowbox.Geom2D.Rasterizer.makeSegments: unsupported ammount of points"
          f2f' = fmap f2f

makeCubics :: (Real a, Fractional a) => Path a -> [CubicBezier a]
makeCubics (Path closed points) = combine points
    where combine [] = []
          combine [a'] = if not closed then [] else let
                  ControlPoint a _ b' = a'
                  ControlPoint d c' _ = head points
                  b = unpackP a d b'
                  c = unpackP d a c'
              in [CubicBezier a b c d]
          combine (a':d':xs) = let
                  ControlPoint a _ b' = a'
                  ControlPoint d c' _ = d'
                  b = unpackP a d b'
                  c = unpackP d a c'
              in CubicBezier a b c d : combine (d':xs)
          combine _ = error "Flowbox.Geom2D.Rasterizer.makeCubics: unsupported ammount of points"

pathToRGBA32 :: (Real a, Fractional a) => Int -> Int -> Path a -> A.Acc (A.Array DIM2 RGBA32)
pathToRGBA32 w h (Path closed points) = imgToRGBA32 rasterize
    where rasterize :: JuicyTypes.Image PixelRGBA8
          rasterize = Rasta.renderDrawing w h (PixelRGBA8 0 0 0 0) $
                          Rasta.withTexture (RastaTex.uniformTexture (PixelRGBA8 255 255 255 255)) $ do
                              let cubics = makeSegments closed points
                              case closed of
                                  False -> Rasta.stroke
                                              4 Rasta.JoinRound (Rasta.CapRound, Rasta.CapRound) $
                                                  fmap (Rasta.transform trans) $ fmap Rasta.CubicBezierPrim cubics
                                  True  -> Rasta.fill $
                                              fmap (Rasta.transform trans) $ fmap Rasta.CubicBezierPrim cubics
          imgToRGBA32 :: JuicyTypes.Image PixelRGBA8 -> A.Acc (A.Array DIM2 RGBA32)
          imgToRGBA32 (Juicy.Image width height vec) = A.use $ fromVectors (Z:.h:.w) ((), (unsafeCast vec))
          trans :: Rasta.Point -> Rasta.Point
          trans (Rasta.V2 x y) = Rasta.V2 x ((y-((fromIntegral h)/2))*(-1)+((fromIntegral h)/2))

pathToMatrix :: (Real a, Fractional a) => Int -> Int -> Path a -> Matrix2 Double
pathToMatrix w h path = extractArr $ pathToRGBA32 w h path
    where extractArr :: A.Acc (A.Array DIM2 RGBA32) -> Matrix2 Double
          extractArr arr = Delayed $ A.map extractVal $ arr
          extractVal :: M.Exp RGBA32 -> M.Exp Double
          extractVal rgba = (A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF) / 255


rasterizeMask :: forall a. (Real a, Fractional a) => Int -> Int -> Mask a -> Matrix2 Double
rasterizeMask w h (Mask path' feather') = -- path
    case feather' of
        Nothing -> path
        Just feather' -> let
                feather = ptm feather'
                convert :: Path a -> A.Acc (A.Vector (QuadraticBezier Double))
                convert p = let
                        a = {-trace ("running makeCubics with p = " ++ show p) $-} makeCubics p
                        quads = {-trace ("running convertCubicsToQuadratics with a = " ++ show ((fmap.fmap) f2d a)) $-} convertCubicsToQuadratics 5 0.001 $ (fmap.fmap) f2d a
                    in {-trace ("running use with quads = " ++ show quads) $-} A.use $ A.fromList (Z :. length quads) quads
                cA = convert path'
                cB = convert feather'
            in M.generate (A.index2 (U.variable h) (U.variable w)) $ combine feather cA cB
    where ptm  = pathToMatrix w h
          path = ptm path'
          combine :: Matrix2 Double -> A.Acc (A.Vector (QuadraticBezier Double)) -> A.Acc (A.Vector (QuadraticBezier Double)) -> A.Exp A.DIM2 -> A.Exp Double
          combine feather pQ fQ idx@(A.unlift . A.unindex2 -> (A.fromIntegral -> y, A.fromIntegral -> x) :: (A.Exp Int, A.Exp Int)) =
              let
                  p  = path M.! idx
                  f  = feather M.! idx
                  h' = A.fromIntegral $ A.lift h
                  d  = distanceFromQuadratics (A.lift $ Point2 x ((y - (h'/2))*(-1) + (h'/2)))
                  dp = d pQ
                  df = d fQ
              --in (1 / (dp/5)) + (1 / (df/5))
              in A.cond (p >* 0.99)
                      (
                        A.cond (0.01 >* f) (dp / (dp+df)) 1
                      )
                      (
                        A.cond (0.01 >* p)
                                (
                                  A.cond (f >* 0.99) (df / (dp+df)) 0
                                )
                                (
                                  A.cond (0.01 >* f)
                                          0
                                          (
                                            A.cond (f >* 0.99) 1 p
                                          )
                                )
                      )
              --in A.cond ((p >* 0.99 &&* f >* 0) ||* (p ==* 0 &&* f ==* 0)) p $ A.cond (p >* 0) (dp / (dp+df)) (df / (dp+df))

matrixToImage :: Matrix2 Float -> Image
matrixToImage a = Image.singleton view
    where view = View.append (Channel.ChannelFloat "rgba.r" $ Channel.MatrixData w)
               $ View.append (Channel.ChannelFloat "rgba.g" $ Channel.MatrixData w)
               $ View.append (Channel.ChannelFloat "rgba.b" $ Channel.MatrixData w)
               $ View.append (Channel.ChannelFloat "rgba.a" $ Channel.MatrixData a)
               $ View.emptyDefault
          w = M.map (\_ -> 1) a

