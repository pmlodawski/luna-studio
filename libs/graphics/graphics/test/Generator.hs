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
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Filter as Conv
import Flowbox.Graphics.Composition.Generators.Gradient
import Flowbox.Graphics.Composition.Generators.Matrix
import Flowbox.Graphics.Composition.Generators.Stencil as Stencil
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
import qualified Math.Coordinate.Polar as Polar
import qualified Math.Coordinate.Cartesian as Cartesian
import Math.Coordinate.Coordinate (convertCoord)
import Data.Array.Accelerate (index2, Boundary(..))

import Utils

--
-- Draws all the existing types of gradients using ContactSheet.
-- (General geometry test)
--
--gradientsTest :: IO ()
--gradientsTest = do
--    let reds   = [Tick 0.0 1.0 1.0, Tick 0.25 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]
--    let greens = [Tick 0.0 1.0 1.0, Tick 0.50 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]
--    let blues  = [Tick 0.0 1.0 1.0, Tick 0.75 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]

--    let alphas = [Tick 0.0 1.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]
--    let gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]

--    let weightFun tickPos val1 weight1 val2 weight2 = mix tickPos val1 val2
--    let mapper = flip colorMapper weightFun
--    let center = translate (V2 90 120) . scale (V2 90 90)
--    let grad1 t = monosampler $ center $ mapper t circularShape
--    let grad2 t = monosampler $ center $ mapper t diamondShape
--    let grad3 t = monosampler $ center $ mapper t squareShape
--    let grad4 t = monosampler $ center $ mapper t conicalShape
--    let grad5 t = monosampler $ center $ mapper t $ radialShape (Minkowski 0.6)
--    let grad6 t = monosampler $ center $ mapper t $ radialShape (Minkowski 3)
--    let grad7 t = monosampler $ mapper t $ scale (V2 180 1) $ linearShape
--    let grad8   = monosampler $ center $ rotate (45 * pi / 180) $ mapper gray conicalShape
    
--    let raster t = gridRasterizer (Grid 720 480) (Grid 4 2) [grad1 t, grad2 t, grad3 t, grad4 t, grad5 t, grad6 t, grad7 t, grad8]

--    testSaveRGBA' "out.bmp" (raster reds) (raster greens) (raster blues) (raster alphas)

--
-- Draws single conical gradient rotated 84deg
-- (Multisampling test)
--
multisamplerTest :: IO ()
multisamplerTest = do
    let gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]
    let mysampler = multisampler (normalize $ toMatrix 10 box)
    let weightFun tickPos val1 weight1 val2 weight2 = mix tickPos val1 val2
    let mapper = flip colorMapper weightFun
    let grad8     = rasterizer $ mysampler $ translate (V2 (720/2) (480/2)) $ rotate (84 * pi / 180) $ mapper gray conicalShape
    testSaveChan' "out.bmp" grad8

--
-- Upcales lena 64x64 image into 720x480 one
-- (Filtering test)
--
scalingTest :: Filter Float -> IO ()
scalingTest flt = do
    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "lena_small.bmp"
    let process x = rasterizer $ monosampler 
                               $ scale (V2 (720 / 64) (480 / 64))
                               $ interpolator flt 
                               $ fromMatrix Clamp x
    testSaveRGBA' "out.bmp" (process r) (process g) (process b) (process a)

--
-- Applies 2 pass gaussian blur onto the 4k image
-- (Convolution regression test)
--
gaussianTest :: Exp Int -> IO ()
gaussianTest kernSize = do
    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "moonbow.bmp"
    let hmat = id M.>-> normalize $ toMatrix (Grid 1 (variable kernSize)) $ gauss 1.0
    let vmat = id M.>-> normalize $ toMatrix (Grid (variable kernSize) 1) $ gauss 1.0
    let p = pipe Clamp
    let process x = rasterizer $ id `p` Conv.filter 1 vmat `p` Conv.filter 1 hmat `p` id $ fromMatrix Clamp x
    testSaveRGBA' "out.bmp" (process r) (process g) (process b) (process a)

--
-- Applies laplacian edge detector to Lena image
-- (Edge detection test)
--
laplacianTest :: Exp Int -> Exp Float -> Exp Float -> IO ()
laplacianTest kernSize crossVal sideVal = do
    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "lena.bmp"
    let flt = laplacian (variable crossVal) (variable sideVal) (pure $ variable kernSize)
    let p = pipe Clamp
    let process x = rasterizer $ id `p` Conv.filter 1 flt `p` id $ fromMatrix Clamp x
    testSaveRGBA' "out.bmp" (process r) (process g) (process b) (process a)

--
-- Applies morphological operators to Lena image
-- (Morphology test)
--
--morphologyTest :: Exp Int -> IO ()
--morphologyTest size = do
--    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "lena.bmp"

--    let l c = monosampler $ scale 0.5 $ nearest $ fromMatrix Clamp c
--    let v = pure $ variable size
--    let morph1 c = closing v $ l c -- Bottom left
--    let morph2 c = opening v $ l c -- Bottom right
--    let morph3 c = dilate  v $ l c -- Top left
--    let morph4 c = erode   v $ l c -- Top right

--    let raster chan = gridRasterizer 512 (Grid 2 2) [morph1 chan, morph2 chan, morph3 chan, morph4 chan]
--    testSaveRGBA' "out.bmp" (raster r) (raster g) (raster b) (raster a)

--
-- Applies directional motion blur to Lena image
-- (Simple rotational convolution)
--

-- Motion blur function should be done in luna instead of Haskell
--motionBlur  :: (IsFloating a, Elt a) => Exp a -> Exp Int -> Matrix2 a -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
--motionBlur alpha scatter kernel = Conv.filter scatter rotKern
--    where rotKern = rotateMat alpha (Constant 0) kernel

--motionBlurTest :: Exp Float -> Exp Int -> IO ()
--motionBlurTest alpha kernSize = do
--    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "lena.bmp"
--    let flt = normalize $ toMatrix (Grid 1 (variable kernSize)) $ box
--    let p = pipe 512 Clamp
--    let process x = rasterizer 512 $ id `p` motionBlur (alpha * pi / 180) 1 flt `p` id $ fromMatrix Clamp x
--    testSaveRGBA' "out.bmp" (process r) (process g) (process b) (process a)


--
-- Applies Kirsch Operator to red channel of Lena image. Available operators: prewitt, sobel, sharr
-- (Advanced rotational convolution, Edge detection test)
--
--rotational :: Exp Float -> Matrix2 Float -> Matrix2 Float -> DiscreteGenerator (Exp Float)
--rotational phi mat chan = id `p` Conv.filter 1 edgeKern `p` id $ fromMatrix Clamp chan
--    where edgeKern = rotateMat (phi * pi / 180) (Constant 0) mat
--          p = pipe 512 Clamp

--kirschTest :: Matrix2 Float -> IO ()
--kirschTest edgeOp = do
--    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "lena.bmp"  
--    let k alpha = rotational alpha edgeOp r
--    let max8 a b c d e f g h = a `min` b `min` c `min` d `min` e `min` f `min` g `min` h
--    let res = rasterizer 512 $ max8 <$> (k 0) <*> (k 45) <*> (k 90) <*> (k 135) <*> (k 180) <*> (k 225) <*> (k 270) <*> (k 315)
--    testSaveChan' "out.bmp" res


--
-- Unsharp mask - FIXME [kl]
-- (Sharpening test)
--
unsharpMaskTest :: Exp Float -> Exp Int -> IO ()
unsharpMaskTest sigma kernSize = do
    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "lena.bmp"
    let flt = normalize $ toMatrix (Grid (variable kernSize) (variable kernSize)) $ dirac (variable sigma) - gauss 1.0
    let p = pipe Clamp
    let process x = rasterizer $ id `p` Conv.filter 1 flt `p` id $ fromMatrix Clamp x
    testSaveRGBA' "out.bmp" (process r) (process g) (process b) (process a)

--
-- Radial blur
-- (Sharpening test)
--
--rotateGen :: (Elt a, IsFloating a, IsScalar a) => Exp a -> Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
--rotateGen phi grid gen = monosampler $ translate (V2 rx ry) 
--                                     $ rotate phi 
--                                     $ translate (V2 tx ty) 
--                                     $ interpolator triangle 
--                                     $ gen
--    where size = fmap A.fromIntegral grid
--          Grid tx ty = fmap (/ (-2)) size
--          Grid rx ry = fmap (/ (2)) $ bbox phi size

--lineGen :: DiscreteGenerator (Exp Float)
--lineGen = Generator $ \(Cartesian.Point2 x y) -> A.cond (y ==* 1) 1 0

--motionBlur' :: (Elt a, IsFloating a) =>  Exp a -> Grid (Exp Int) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
--motionBlur' alpha grid kernel = Stencil.normStencil (+) (fmap A.ceiling $ bbox alpha $ fmap A.fromIntegral grid) (rotateGen alpha grid kernel) (+) 0

--radialBlurTest :: IO ()
--radialBlurTest = do
--    (r :: Matrix2 Float, g, b, a) <- testLoadRGBA' "lena.bmp"

--    let remap = S.transform $ \(Polar.Point2 x y) -> Cartesian.Point2 x (y / (2 * pi) * 512)

--    let remap' = S.transform $ \(Cartesian.Point2 x' y') -> Polar.Point2 x' (y')

--    let flt = normalize $ toMatrix 10 $ gauss 1.0

--    let wrap x = monosampler
--               $ translate (256)
--               $ S.transform (Polar.toPolar (512 :: Grid (Exp Float))) 
--               $ remap
--               $ translate (-256)
--               $ nearest x
               

--    let unwrap x = Conv.filter 2 flt 
--                 $ monosampler 
--                 $ translate (256)
--                 $ S.transform (\(Cartesian.Point2 x y) -> Polar.Point2 x (y * (2 * pi) / 512))
--                 $ S.transform (convertCoord Cartesian.Cartesian (512 :: Grid (Exp Float))) 
--                 $ translate (-256)
--                 $ nearest
--                 $ fromMatrix (Clamp) x

--    let process = rasterizer 512 . wrap . unwrap
--    print "Szatan"
--    testSaveRGBA' "out.bmp" (process r) (process g) (process b) (process a)

main :: IO ()
main = do
    putStrLn "Running gauss 50x50..."
    gaussianTest (50 :: Exp Int)
