---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE CPP          #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Flowbox.Prelude hiding (zoom, constant, transform, from, min, max, over, under)
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Matrix
import Flowbox.Graphics.Composition.Generators.Rasterizer
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Image.Merge

import qualified Data.Array.Accelerate as A
import           Data.Foldable
import           Math.Space.Space
import           System.IO             (hSetBuffering, stdout, BufferMode(..))

import Utils

merge file mode alphaBlending = do
    (r1, g1, b1, a1) <- map4 (nearest . fromMatrix A.Wrap) <$> testLoadRGBA' "samples/lena_alpha.png"
    (r2, g2, b2, a2) <- map4 (nearest . fromMatrix A.Wrap) <$> testLoadRGBA' "samples/checker.png"
    let (r, g, b, a) = map4 (rasterizer . monosampler) $ threeWayMerge' alphaBlending mode r1 g1 b1 r2 g2 b2 a1 a2
    testSaveRGBA'' file r g b a

merge' file mode = do
    (r1, g1, b1, a1) <- map4 (nearest . fromMatrix A.Wrap) <$> testLoadRGBA' "samples/lena_alpha.png"
    (r2, g2, b2, a2) <- map4 (nearest . fromMatrix A.Wrap) <$> testLoadRGBA' "samples/checker.png"
    let (r, g, b, a) = map4 (rasterizer . monosampler) $ threeWayMerge mode r1 g1 b1 r2 g2 b2 a1 a2
    testSaveRGBA'' file r g b a

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
    hSetBuffering stdout NoBuffering
    putStrLn "Merge test"

    putStrLn "Simple merges with Adobe"
    forM_ (zip simpleMergesNames simpleMerges) $ \(m, n) -> do
        putStr $ "Merge " ++ m ++ "..."
        merge ("samples/merge/" ++ m ++ ".png") n Adobe
        putStrLn " DONE"

    putStrLn "Simple merges with Custom"
    forM_ (zip simpleMergesNames simpleMerges) $ \(m, n) -> do
        putStr $ "Merge " ++ m ++ "..."
        merge ("samples/merge/" ++ m ++ "_alphablend.png") n Custom
        putStrLn " DONE"

    putStrLn "Complicated merges"
    forM_ (zip advancedMergesNames advancedMerges) $ \(m, n) -> do
        putStr $ "Merge " ++ m ++ "..."
        merge' ("samples/merge/" ++ m ++ ".png") n
        putStrLn " DONE"
