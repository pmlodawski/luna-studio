---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Flowbox.Prelude hiding (zoom)
import Flowbox.Graphics.Composition.Generators.Gradient
import Flowbox.Graphics.Composition.Generators.Lambda
import Flowbox.Graphics.Composition.Generators.Multisampler
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Transform
import Flowbox.Graphics.Utils
import Flowbox.Math.Matrix as M
import Data.Array.Accelerate as A hiding (rotate)
import Data.Array.Accelerate.CUDA

import Linear.V2
import Math.Space.Space
import Math.Metric
import Math.Coordinate.Cartesian
import Data.Array.Accelerate (index2, Boundary(..))

import Utils

saveGen :: Generator -> IO ()
saveGen gen = do
    let res = lambdaGenerator (Grid 640 480) gen
    testSaveRGBA "out.bmp" res res res res

scaling f = do
    (r, g, b, a) <- testLoadRGBA "lena.bmp"
    let process x = lambdaGenerator (Grid 500 500) $ translate (V2 1.5 1.5) $ bicubic f (Constant 1.0) x
    testSaveRGBA "out.bmp" (process r) (process g) (process b) (process a)

szatan :: (t -> Generator -> Generator) -> (Exp Double -> t) -> Generator -> Generator -> Generator
szatan transformation converter mask source = Generator $ \pixel space -> let arg = converter $ runGenerator mask pixel space
                                                                          in runGenerator (transformation arg source) pixel space

mydisort :: Generator -> Generator -> Generator
mydisort = szatan translate $ \x -> V2 0 x

disortion = do
    (r, g, b, a) <- testLoadRGBA "bar.bmp"
                -- pozycja, wartość, waga
    let ticks = [Tick 0.0 0.0 1.0, Tick 0.5 100.0 5.0, Tick 1.0 0.0 1.0]

    --let myftrans pw nw prop = 0.5 + (atan $ nw * pw * (prop - 0.5)) / (2 * atan (nw * pw))
    let myftrans pw nw prop = let n = pw * nw in prop ** n / (prop ** n + (1 - prop) ** n)

    let sampler    = translate (V2 (-256) (-256)) . bicubic triangle (Constant 0.5)
    --let grad       = zoom (V2 64 64) $ colorMapper ticks myftrans $ circularShape
    let grad       = translate (V2 (-256) (-256)) . colorMapper ticks myftrans $ linearShape
    let effect     = translate (V2 256 256) . mydisort grad
    let rasterizer = lambdaGenerator (Grid 512 512)
    let process    = rasterizer .  effect . sampler

    testSaveRGBA "out.bmp" (process r) (process g) (process b) (process a)

main :: IO ()
main = do
    putStrLn "Testuję skalowanie"
    scaling lanczos3
