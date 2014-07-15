---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module Main where

import Flowbox.Prelude hiding (zoom, constant, transform, from, min, max, over, under)
--import Flowbox.Graphics.Composition.Generators.Constant
import Flowbox.Graphics.Composition.Generators.Filter
--import Flowbox.Graphics.Composition.Generators.Gradient
--import Flowbox.Graphics.Composition.Generators.Multisampler
import Flowbox.Graphics.Composition.Generators.Rasterizer
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Structures
--import Flowbox.Graphics.Composition.Generators.Transform
import Flowbox.Graphics.Image.IO.ImageMagick
import Flowbox.Graphics.Image.Merge
--import Flowbox.Graphics.Utils
import Flowbox.Math.Matrix as M hiding ((++), stencil, zip)
--import Data.Array.Accelerate as A hiding (rotate, constant)
--import Data.Array.Accelerate.CUDA

--import Linear.V2
import Math.Space.Space
--import Math.Metric
--import Math.Coordinate.Cartesian
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA
import qualified Data.Array.Accelerate.IO as AIO
import Data.Foldable

import Utils

merge file mode alphaBlending = do
    (r1, g1, b1, a1) <- map4 (nearest A.Wrap) <$> testLoadRGBA' "lena_premult.png"
    (r2, g2, b2, a2) <- map4 (nearest A.Wrap) <$> testLoadRGBA' "checker_premult_constalpha.png"
    let merge' ov bg = rasterizer (Grid 512 512) $ transform (fmap A.fromIntegral) $ basicColorCompositingFormula ov a1 bg a2 alphaBlending mode
    testSaveRGBA'' file (merge' r1 r2) (merge' g1 g2) (merge' b1 b2) (merge' a1 a2)

merge' file mode = do
    (r1, g1, b1, a1) <- map4 (nearest A.Wrap) <$> testLoadRGBA' "lena_premult.png"
    (r2, g2, b2, a2) <- map4 (nearest A.Wrap) <$> testLoadRGBA' "checker_premult_constalpha.png"
    let merge' ov bg = rasterizer (Grid 512 512) $ transform (fmap A.fromIntegral) $ complicatedColorCompositingFormula ov a1 bg a2 mode
    testSaveRGBA'' file (merge' r1 r2) (merge' g1 g2) (merge' b1 b2) (merge' a1 a2)

map4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4 f (a, b, c, d) = (f a, f b, f c, f d)

simpleMerges :: [BlendMode Double]
simpleMerges = [
      average
    , colorDodge
    , colorBurn
    , copy
    , difference
    , divideByDst
    , exclusion
    , from
    , geometric
    , hardLight
    , hypot
    , max
    , min
    , minus
    , multiply
    , plus
    , screen
    , overlayFun
    , softLight
    ]


simpleMergesNames :: [String]
simpleMergesNames = [
      "average"
    , "colorDodge"
    , "colorBurn"
    , "copy"
    , "difference"
    , "divideByDst"
    , "exclusion"
    , "from"
    , "geometric"
    , "hardLight"
    , "hypot"
    , "max"
    , "min"
    , "minus"
    , "multiply"
    , "plus"
    , "screen"
    , "overlayFun"
    , "softLight"
    ]

advancedMerges :: [ComplicatedBlendMode Double]
advancedMerges = [
      atop
    , conjointOver
    , disjointOver
    , inBlend
    , withMask
    , matte
    , out
    , over
    , stencil
    , under
    , xor
    ]

advancedMergesNames :: [String]
advancedMergesNames = [
      "atop"
    , "conjointOver"
    , "disjointOver"
    , "inBlend"
    , "withMask"
    , "matte"
    , "out"
    , "over"
    , "stencil"
    , "under"
    , "xor"
    ]

main :: IO ()
main = do
    putStrLn "Merge test"
    --putStrLn "Simple merges with Adobe"
    --forM_ (zip simpleMergesNames simpleMerges) $ \(m, n) -> do
    --    !a <- return $ run $ A.generate (A.index2 (512::A.Exp Int) 512) (\_ -> 1.0 :: A.Exp Float)
    --    merge (m ++ ".png") n Adobe

    --putStrLn "Simple merges with Custom"
    --forM_ (zip simpleMergesNames simpleMerges) $ \(m, n) -> do
    --    !a <- return $ run $ A.generate (A.index2 (512::A.Exp Int) 512) (\_ -> 0.0 :: A.Exp Float)
    --    merge (m ++ "_alphablend.png") n Custom

    --putStrLn "Complicated merges"
    --forM_ (zip advancedMergesNames advancedMerges) $ \(m, n) -> do
    --    !a <- return $ run $ A.generate (A.index2 (512::A.Exp Int) 512) (\_ -> 0.5 :: A.Exp Float)
    --    merge' (m ++ ".png") n

    let softlights = [softLight, softLightPegtop, softLightIllusions, softLightPhotoshop]
        softlightsNames = ["softLight", "softLight_prim", "softLight_bis", "softLight_tetr"]

    forM_ (zip softlightsNames softlights) $ \(m, n) -> do
        merge (m ++ ".png") n Adobe
