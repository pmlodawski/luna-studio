---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Flowbox.Graphics.Mockup
import           Flowbox.Graphics.Mockup.Basic as Mockup
import           Flowbox.Graphics.Mockup.Transform as Transform
import qualified Flowbox.Math.Matrix as M
import           Data.Array.Accelerate as A hiding (fromIntegral)
import           Data.Array.Accelerate.IO

import           Flowbox.Geom2D.ControlPoint
import           Flowbox.Geom2D.Path
import           Flowbox.Geom2D.Mask as Mask
import           Flowbox.Geom2D.Rasterizer hiding (makePoints, makeSegments)
import           Flowbox.Graphics.Image.IO.BMP
import           Flowbox.Graphics.Color.RGBA as Color
import           Flowbox.Math.Function.Accelerate.BSpline
import qualified Flowbox.Graphics.Composition.Color as Color
import           Flowbox.Graphics.Color.Color as Color
import           Flowbox.Graphics.Image.View as View
import           Data.Maybe
import           Flowbox.Graphics.Image.Channel
import           Flowbox.Prelude as P hiding (lookup)
import           Flowbox.Graphics.Image.Image
import qualified Flowbox.Graphics.Mockup as Mockup
import           Flowbox.Graphics.Shader.Shader
import           Flowbox.Graphics.Image.Matte
import           Flowbox.Graphics.Composition.Generator.Gradient
import           Flowbox.Graphics.Composition.Transform
import           Flowbox.Graphics.Image.View as View
import           Flowbox.Graphics.Image.Image
import           Flowbox.Graphics.Shader.Sampler
import           Flowbox.Graphics.Shader.Rasterizer
import           Flowbox.Graphics.Shader.Shader as Shader
import           Flowbox.Math.Matrix as M
import qualified Data.Array.Accelerate.CUDA as CUDA


dist :: (Point2 (A.Exp Double)) -> A.Exp Double
dist (Point2 x y) = c
    where
        a = (256-x)*(256-x) + (256-y)*(256-y)
        b = a*a
        c = b / 130000.0

backend :: M.Backend
backend = CUDA.run

main :: IO ()
main = do
    print "- - - = = =   Transformations Test  = = = - - -"

    --img <- loadImageLuna "lena.png"

    --let t = Transform (V2 50.0 200.0) (pi/2) (V2 1.5 1.8) (Skew (V2 0.0 1.3) SkewYX) (Point2 300.0 100.0)
    --let img1 = Transform.transformLuna t img
    --saveImageLuna "./t1.png" img1

    --let img2 = scaleAtLuna (Point2 180.0 100.0) (V2 0.9 0.6) (Nothing) img
    --saveImageLuna "./t2.png" img2

    --let img11 = skewAtLuna (Point2 250.0 150.0) (Skew (V2 1.5 0.5) SkewYX) (Nothing) img
    --saveImageLuna "./t3.png" img11

    --let img4 = translateLuna (V2 140.0 50.0) (Nothing) img
    --saveImageLuna "./t4.png" img4

    --let img5 = skewAtLuna (Point2 100.0 150.0) (Skew (V2 1.0 1.0) SkewXY) (Nothing) img
    --saveImageLuna "./t5.png" img5

    --let img6 = rotateAtLuna (Point2 200.0 400.0) (pi/3) (Nothing) img
    --saveImageLuna "./t6.png" img6

    print "done"
