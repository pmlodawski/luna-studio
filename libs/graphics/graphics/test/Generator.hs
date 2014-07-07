---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Flowbox.Prelude hiding (zoom, constant)
import Flowbox.Graphics.Composition.Generators.Constant
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Gradient
import Flowbox.Graphics.Composition.Generators.Multisampler
import Flowbox.Graphics.Composition.Generators.Rasterizer
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Transform
import Flowbox.Graphics.Utils
import Flowbox.Math.Matrix as M
import Data.Array.Accelerate as A hiding (rotate, constant)
import Data.Array.Accelerate.CUDA

import Linear.V2
import Math.Space.Space
import Math.Metric
import Math.Coordinate.Cartesian
import Data.Array.Accelerate (index2, Boundary(..))

--import Utils

<<<<<<< Updated upstream
gradients = do
    let reds   = [Tick 0.0 1.0 1.0, Tick 0.25 0.0 1.0, Tick 1.0 1.0 1.0]
    let greens = [Tick 0.0 1.0 1.0, Tick 0.50 0.0 1.0, Tick 1.0 1.0 1.0]
    let blues  = [Tick 0.0 1.0 1.0, Tick 0.75 0.0 1.0, Tick 1.0 1.0 1.0]

    let alphas = [Tick 0.0 1.0 1.0, Tick 1.0 1.0 1.0]
    let gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0]

    let myftrans pw nw prop = prop ** (pw / nw)
    let mapper = flip colorMapper myftrans
    let center = translate (V2 90 120) . scale (V2 90 90)
    let grad1 t = center $ mapper t circularShape
    let grad2 t = center $ mapper t diamondShape
    let grad3 t = center $ mapper t squareShape
    let grad4 t = center $ mapper t conicalShape
    let grad5 t = center $ mapper t $ radialShape (Minkowski 0.6)
    let grad6 t = center $ mapper t $ radialShape (Minkowski 3)
    let grad7 t = mapper t $ linearShape
    let grad8   = center $ rotate (84/180 * pi) $ mapper gray conicalShape

    let raster t = gridRasterizer (Grid 720 480) (Grid 4 2) [grad1 t, grad2 t, grad3 t, grad4 t, grad5 t, grad6 t, grad7 t, grad8]

    testSaveRGBA "out.bmp" (raster reds) (raster greens) (raster blues) (raster alphas)

main :: IO ()
main = do
    putStrLn "Gradient test"
    gradients
=======
--saveGen :: Generator -> IO ()
--saveGen gen = do
--    let res = lambdaGenerator (Grid 640 480) gen
--    testSaveRGBA "out.bmp" res res res res

--scaling f = do
--    (r, g, b, a) <- testLoadRGBA "lena.bmp"
--    let process x = lambdaGenerator (Grid 500 500) $ translate (V2 1.5 1.5) $ bicubic f (Constant 1.0) x
--    testSaveRGBA "out.bmp" (process r) (process g) (process b) (process a)

--disortion :: (Exp Double, Exp Double, Exp Double, Exp Double) -> IO ()
--disortion (x1, y1, x2, y2) = do
--    (r, g, b, a) <- testLoadRGBA "lena.bmp"

--    let vec = variable $ V2 (Point2 x1 y1) (Point2 x2 y2)
--                -- pozycja, wartość, waga
--    let ticks = [Tick 0.0 1 1.0, Tick 0.5 (-0.5) 1.0, Tick 1.0 2 1.0]
--    let myftrans pw nw prop = prop ** (pw / nw)

--    let tv = V2 x1 y2
--    let sampler    = bicubic triangle Wrap
--    let grad       = colorMapper ticks myftrans $ radialShape (Chebyshev)
--    let effect     = disort (Point2 0 0) grad . translate (tv)
--    let rasterizer = lambdaGenerator (Grid 512 512)
--    let process    = rasterizer . effect . sampler

--    let gradient ticks = rasterizer grad

--    testSaveRGBA "out.bmp" (process r) (process g) (process b) (process a)
--    testSaveRGBA "out_grad.bmp" (gradient ticks) (gradient ticks) (gradient ticks) (gradient ticks)

main :: IO ()
main = do
    putStrLn "Testuję skalowanie"
    --scaling lanczos3
>>>>>>> Stashed changes
