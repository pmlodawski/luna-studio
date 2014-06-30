---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Main where

import Flowbox.Prelude
import Flowbox.Graphics.Composition.Generators.Gradient
import Flowbox.Graphics.Composition.Generators.Lambda
import Flowbox.Graphics.Composition.Generators.Multisampler
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Transform

import Linear.V2
import Math.Space.Space
import Math.Metric
import Math.Coordinate.Cartesian
import Data.Array.Accelerate (index2, Boundary(..))

import Utils

gradient (w, h) (x1, y1, x2, y2) shape = do
    let reds   = [Tick 0.0 1.0 1.0, Tick 0.25 0.0 1.0, Tick 1.0 1.0 1.0]
    let greens = [Tick 0.0 1.0 1.0, Tick 0.50 0.0 1.0, Tick 1.0 1.0 1.0]
    let blues  = [Tick 0.0 1.0 1.0, Tick 0.75 0.0 1.0, Tick 1.0 1.0 1.0]
    let alphas = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0]
    let myftrans pw nw prop = prop ** (pw / nw)
    
    let vec = V2 (Point2 x1 y1) (Point2 x2 y2)
    let gradient ticks = lambdaGenerator (Grid w h) $ colorMapper vec ticks myftrans shape

    testSaveRGBA "out.bmp" (gradient alphas) (gradient alphas) (gradient alphas) (gradient alphas)

--gauss w h sigma = do
--    let gauss = lambdaGenerator (Grid w h) gaussian
--    testSaveChan "out.bmp" gauss

scaling f = do
    (r, g, b, a) <- testLoadRGBA "lena.bmp"
    let process x = lambdaGenerator (Grid 500 500) $ translate (V2 1.5 1.5) $ bicubic f (Constant 1.0) x
    testSaveRGBA "out.bmp" (process r) (process g) (process b) (process a)

disortion strength zoom = do
    (r, g, b, a) <- testLoadRGBA "disorted.bmp"
    let process x = lambdaGenerator (Grid 600 400) $ lensDisort (Point2 300 200) strength zoom $ bicubic lanczos3 (Constant 0.0) x
    testSaveRGBA "out.bmp" (process r) (process g) (process b) (process a)

main :: IO ()
main = do
    putStrLn "Testuję skalowanie"
    scaling lanczos3
