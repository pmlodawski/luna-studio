---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Flowbox.Graphics.Mockup as Mockup
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
import           Flowbox.Graphics.Composition.Transform as Transform
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

    img <- loadImageLuna "lena.png"

    let result2 = rotateAtMatteLuna (Point2 512.0 0.0) (pi) (Nothing) img
    let result3 = scaleAtMatteLuna (Point2 256.0 256.0) (V2 1.0 1.0) (Nothing) img

    saveImageLuna "./output2.png" result2
    saveImageLuna "./output3.png" result3

--data SkewOrder = SkewXY | SkewYX

--data Skew a = Skew { _skewValue :: V2 a
--                   , _skewOrder :: SkewOrder
--                   }

--data Transform a = Transform { _translate :: V2 a
--                             , _rotate    :: a
--                             , _scale     :: V2 a
--                             , _skew      :: Skew a
--                             , _center    :: Point2 a
--                             }

    let s1 = Skew (V2 1.0 1.0) SkewXY
    let s2 = Skew (V2 1.0 1.0) SkewYX
    let s3 = Skew (V2 1.0 0.0) SkewXY
    let s4 = Skew (V2 0.0 1.0) SkewXY

    let tr1 = Transform (V2 0.0 0.0) 0.0 (V2 1.0 1.0) s1 (Point2 256.0 256.0)
    let tr2 = Transform (V2 0.0 0.0) 0.0 (V2 1.0 1.0) s2 (Point2 256.0 256.0)
    let tr3 = Transform (V2 0.0 0.0) 0.0 (V2 1.0 1.0) s3 (Point2 256.0 256.0)
    let tr4 = Transform (V2 0.0 0.0) 0.0 (V2 1.0 1.0) s4 (Point2 256.0 256.0)

    let skew1 = transformLuna tr1 (Nothing) img
    let skew2 = transformLuna tr2 (Nothing) img
    let skew3 = transformLuna tr3 (Nothing) img
    let skew4 = transformLuna tr4 (Nothing) img

    saveImageLuna "./skew1.png" skew1
    saveImageLuna "./skew2.png" skew2
    saveImageLuna "./skew3.png" skew3
    saveImageLuna "./skew4.png" skew4


    let tr5 = Transform (V2 100.0 100.0) (pi/2) (V2 2.0 2.0) (Skew (V2 1.0 1.0) SkewYX) (Point2 256.0 256.0)
    let img5 = transformLuna tr5 (Nothing) img
    saveImageLuna "./img5.png" img5

    let tr6 = Transform (V2 100.0 100.0) (0.0) (V2 1.0 1.0) (Skew (V2 0.0 0.0) SkewXY) (Point2 256.0 256.0)
    let img6 = transformLuna tr6 (Nothing) img5
    saveImageLuna "./img6.png" img6

    let tr7 = Transform (V2 100.0 100.0) (pi/4) (V2 2.0 2.0) (Skew (V2 1.0 1.0) SkewXY) (Point2 256.0 256.0)
    let img7 = transformLuna tr7 (Nothing) img
    saveImageLuna "./img7.png" img7

    let tr8 = Transform (V2 100.0 100.0) (pi/4) (V2 1.0 1.0) (Skew (V2 0.0 0.0) SkewXY) (Point2 256.0 256.0)
    let img8 = transformLuna tr8 (Nothing) img
    saveImageLuna "./img8.png" img8
  --  saveImageLuna "./output4.png" result4


    --let f = (\x -> (1.0 - (1.0 / (1.0 + (dist x)))))

    --let shader = Shader (Grid 512 512) f

    --let shader1 = gaussianShape
    --let matte1 = ImageMatte (ChannelFloat "" (ContinuousData shader))

    --let result = scaleMatteLuna (V2 0.5 0.5)  (Just matte1) img
    --saveImageLuna "./output.png" result

    --let shader2 = linearShape
    --let matte2 = ImageMatte (ChannelFloat "" (ContinuousData shader2))

    --let shader3 = diamondShape
    --let matte3 = ImageMatte (ChannelFloat "" (ContinuousData shader3))

    --let shader4 = squareShape
    --let matte4 = ImageMatte (ChannelFloat "" (ContinuousData shader4))  

    --let shader5 = circularShape
    --let matte5 = ImageMatte (ChannelFloat "" (ContinuousData shader5))

    --print (f (Point2 100.0 100.0))

    --let result4 = rotateAtMatteLuna (Point2 256.0 256.0) (pi/4) (Nothing) img
    --saveImageLuna "./output4.png" result4

    --let result5 = rotateAtMatteLuna (Point2 256.0 256.0) (pi/4) (Just matte2) img
    --saveImageLuna "./output5.png" result5

    --let result6 = rotateAtMatteLuna (Point2 256.0 256.0) (pi/4) (Just matte3) img
    --saveImageLuna "./output6.png" result6  

    --let result7 = rotateAtMatteLuna (Point2 256.0 256.0) (pi/4) (Just matte4) img
    --saveImageLuna "./output7.png" result7 

    --let result8 = rotateAtMatteLuna (Point2 256.0 256.0) (pi/4) (Just matte5) img
    --saveImageLuna "./output8.png" result8

    --let v = V2 256.0 256.0
    --let shader6 = Transform.translate v shader5
    --let o = Shader.transform (\x -> x) shader6

    --let matte6 = ImageMatte (ChannelFloat "" (ContinuousData o))
    --let (Shader _ fu) = o

    --print (fu (Point2 256 256))
    --let fu' = (\(p@(Point2 x y)) -> (x >* 270 &&* x <=* 270 &&* x >=* 230 &&* y <=* 270 &&* y >=* 230) A.? (0.0, (fu p) / 20) )
    --let s = Shader (Grid 512 512) fu'
    --let matte7 = ImageMatte (ChannelFloat "" (ContinuousData s))

    --let result9 = rotateAtMatteLuna (Point2 256.0 256.0) (pi/4) (Just matte7) img
    --saveImageLuna "./output9.png" result9

    --let sh = Shader (Grid 512 512) fu

    --let chan1 = ChannelFloat "rgba.r" (mapOverData (\x -> x/500) (ContinuousData sh))
    --let chan2 = ChannelFloat "rgba.g" (mapOverData (\x -> x/500) (ContinuousData sh))
    --let chan3 = ChannelFloat "rgba.b" (mapOverData (\x -> x/500) (ContinuousData sh))
    --let chan4 = ChannelFloat "rgba.a" (mapOverData (\x -> 1.0) (ContinuousData sh))

    --let image = singletonFromChans [chan1, chan2, chan3, chan4]
    --saveImageLuna "./mask.png" image


    print "done"
