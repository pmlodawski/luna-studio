---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Flowbox.Prelude as P hiding (zoom, constant)

import Data.Array.Accelerate as A hiding (rotate, constant)
import Data.Array.Accelerate.CUDA

import Flowbox.Graphics.Composition.Generators.Constant
import Flowbox.Graphics.Composition.Generators.Convolution as Conv
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Gradient
import Flowbox.Graphics.Composition.Generators.Pipe
import Flowbox.Graphics.Composition.Generators.Rasterizer
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Structures as S
import Flowbox.Graphics.Composition.Generators.Transform

import Flowbox.Math.Matrix as M
import Flowbox.Graphics.Utils

import Linear.V2
import Math.Space.Space
import Math.Metric
import Math.Coordinate.Cartesian
import Data.Array.Accelerate (index2, Boundary(..))

import Utils



gradients x = do
    let reds   = [Tick 0.0 1.0 1.0, Tick 0.25 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]
    let greens = [Tick 0.0 1.0 1.0, Tick 0.50 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]
    let blues  = [Tick 0.0 1.0 1.0, Tick 0.75 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]

    let alphas = [Tick 0.0 1.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]
    let gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]

    let weightFun tickPos val1 weight1 val2 weight2 = mix tickPos val1 val2
    let mapper = flip colorMapper weightFun
    let center = translate (V2 90 120) . scale (V2 (variable x) (variable x))
    let grad1 t = monosampler $ center $ mapper t circularShape
    let grad2 t = monosampler $ center $ mapper t diamondShape
    let grad3 t = monosampler $ center $ mapper t squareShape
    let grad4 t = monosampler $ center $ mapper t conicalShape
    let grad5 t = monosampler $ center $ mapper t $ radialShape (Minkowski 0.6)
    let grad6 t = monosampler $ center $ mapper t $ radialShape (Minkowski 3)
    let grad7 t = monosampler $ mapper t $ scale (V2 180 1) $ linearShape

    let mysampler = multisampler (normalize $ toMatrix 10 box)
    let grad8     = mysampler $ center $ rotate (84/180 * pi) $ mapper gray conicalShape
    
    let raster t = gridRasterizer (Grid 720 480) (Grid 4 2) [grad1 t, grad2 t, grad3 t, grad4 t, grad5 t, grad6 t, grad7 t, grad8]

    testSaveRGBA' "out.bmp" (raster reds) (raster greens) (raster blues) (raster alphas)

gradient = do
    let gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]
    let mysampler = multisampler (normalize $ toMatrix 10 box)
    let weightFun tickPos val1 weight1 val2 weight2 = mix tickPos val1 val2
    let mapper = flip colorMapper weightFun
    let grad8     = rasterizer (Grid 720 480) $ mysampler $ translate (V2 (720/2) (480/2)) $ rotate (84/180 * pi) $ mapper gray conicalShape
    testSaveChan' "out.bmp" grad8

scaling flt ang = do
    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "rings.bmp"
    let mysampler = multisampler (normalize $ toMatrix 100 triangle)
    let process x = rasterizer 1000 $ mysampler $ scale (pure $ variable ang) $ (nearest $ fromMatrix Clamp x :: Generator (Exp Float) (Exp Float))
    testSaveRGBA' "out.bmp" (process r) (process g) (process b) (process a)

small :: Exp Int -> Exp Float -> Exp Float -> IO ()
small x y z = do
    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "lena.bmp"
    let flt = laplacian (variable y) (variable z) (pure $ variable x)
    let p = pipe 512 Clamp
    let process x = rasterizer 512 $ id `p` Conv.filter 1 flt `p` id $ fromMatrix Clamp x
    testSaveRGBA' "out.bmp" (process r) (process g) (process b) (process a)

filters :: Exp Int -> IO ()
filters x = do
    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "moonbow.bmp"
    let hmat = id M.>-> normalize $ toMatrix (Grid 1 (variable x)) $ gauss 1.0
    let vmat = id M.>-> normalize $ toMatrix (Grid (variable x) 1) $ gauss 1.0
    let p = pipe (Grid 4096 2304) Clamp
    let process x = rasterizer (Grid 4096 2304) $ id `p` Conv.filter 1 vmat `p` Conv.filter 1 hmat `p` id $ fromMatrix Clamp x
    testSaveRGBA' "out.bmp" (process r) (process g) (process b) (process a)

--rotational :: Exp Float -> Matrix2 Float -> IO (DiscreteGenerator (Exp Float))
rotational phi mat = id `p` Conv.filter 1 edgeKern `p` id $ fromMatrix Clamp mat
    where size' = Grid 3 (3 :: Exp Int)
          size = fmap A.fromIntegral size'
          Grid tx ty = fmap (/ (-2)) size
          nsize = bbox phi size
          nsize' = fmap A.round nsize
          Grid rx ry = fmap (/ (2)) nsize

          flt = scharr

          edgeKern = rasterizer nsize' $ monosampler 
                                       $ translate (V2 rx ry) 
                                       $ rotate phi 
                                       $ translate (V2 tx ty) 
                                       $ interpolator triangle 
                                       $ fromMatrix (Constant 0) $ flt
          p = pipe (Grid 512 512) Clamp

mytest = do
    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "lena.bmp"  

    let mat1 = rotational (  0 * pi / 180) r
    let mat2 = rotational ( 45 * pi / 180) r
    let mat3 = rotational ( 90 * pi / 180) r
    let mat4 = rotational (135 * pi / 180) r
    let mat5 = rotational (180 * pi / 180) r
    let mat6 = rotational (225 * pi / 180) r
    let mat7 = rotational (270 * pi / 180) r
    let mat8 = rotational (315 * pi / 180) r
    let max8 a b c d e f g h = a `min` b `min` c `min` d `min` e `min` f `min` g `min` h

    let res = rasterizer (Grid 512 512) $ max8 <$> mat1 <*> mat2 <*> mat3 <*> mat4 <*> mat5 <*> mat6 <*> mat7 <*> mat8
    testSaveChan' "out.bmp" res

main :: IO ()
main = do
    putStrLn "Gradient test"
    filters (90 :: Exp Int)
