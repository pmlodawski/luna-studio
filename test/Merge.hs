---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Flowbox.Graphics.Composition.Merge
import Flowbox.Graphics.Shader.Matrix
import Flowbox.Graphics.Shader.Rasterizer
import Flowbox.Graphics.Shader.Sampler
import Flowbox.Graphics.Shader.Shader
import Flowbox.Prelude                    hiding (from, max, min, over, transform, under, zoom)

import qualified Data.Array.Accelerate as A
import           Data.Foldable

import Utils



type DtC = DiscreteShader (A.Exp Float) -> ContinuousShader (A.Exp Float)
type CtD = ContinuousShader (A.Exp Float) -> DiscreteShader (A.Exp Float)

merge :: FilePath -> BlendMode Float -> AlphaBlend -> IO ()
merge file mode alphaBlending = do
    (r1, g1, b1, a1) <- map4 ((nearest :: DtC) . fromMatrix A.Wrap) <$> testLoadRGBA' "samples/lena_alpha.png"
    (r2, g2, b2, a2) <- map4 ((nearest :: DtC) . fromMatrix A.Wrap) <$> testLoadRGBA' "samples/checker.png"
    let (r, g, b, a) = map4 (rasterizer . (monosampler :: CtD)) $ threeWayMerge' alphaBlending mode r1 g1 b1 r2 g2 b2 a1 a2
    testSaveRGBA'' file r g b a

merge' :: FilePath -> ComplicatedBlendMode Float -> IO ()
merge' file mode = do
    (r1, g1, b1, a1) <- map4 ((nearest :: DtC) . fromMatrix A.Wrap) <$> testLoadRGBA' "samples/lena_alpha.png"
    (r2, g2, b2, a2) <- map4 ((nearest :: DtC) . fromMatrix A.Wrap) <$> testLoadRGBA' "samples/checker.png"
    let (r, g, b, a) = map4 (rasterizer . (monosampler :: CtD)) $ threeWayMerge mode r1 g1 b1 r2 g2 b2 a1 a2
    testSaveRGBA'' file r g b a

simpleMerges :: [BlendMode Float]
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
    , softLightPegtop
    , softLightIllusions
    , softLightPhotoshop
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
    , "softLightPegtop"
    , "softLightIllusions"
    , "softLightPhotoshop"
    ]

advancedMerges :: [ComplicatedBlendMode Float]
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

    putStrLn "Simple merges with Adobe"
    forM_ (zip simpleMergesNames simpleMerges) $ \(m, n) -> do
        merge (m ++ ".png") n Adobe

    putStrLn "Simple merges with Custom"
    forM_ (zip simpleMergesNames simpleMerges) $ \(m, n) -> do
        merge (m ++ "_alphablend.png") n Custom

    putStrLn "Complicated merges"
    forM_ (zip advancedMergesNames advancedMerges) $ \(m, n) -> do
        merge' (m ++ ".png") n

