module Flowbox.Graphics.MockupSpec where

import Test.Hspec
import Test.QuickCheck
import Flowbox.Graphics.Mockup as M
import Flowbox.Graphics.Composition.EdgeBlur as EB
import Flowbox.Graphics.Composition.Dither   as D
import Flowbox.Graphics.Composition.Generator.Gradient
import Flowbox.Graphics.Shader.Sampler
import Flowbox.Graphics.Shader.Rasterizer
import Flowbox.Graphics.Shader.Shader                  as S
import Flowbox.Graphics.Composition.Transform
import qualified Flowbox.Math.Matrix as M
import Data.Array.Accelerate.CUDA as A
import qualified Data.Array.Accelerate              as A
import System.IO.Unsafe

import Flowbox.Prelude as P

import TestHelpers


spec :: Spec
spec = do
    let specPath = "./Flowbox/Graphics/"
        in do let testName = "edgeBlur"
                in describe testName $ do 
                    describe "should match previously computed picture on rgba.r channel with edge multiplier 5 and kernel size 5" $ do
                        let actualImage = unsafePerformIO $ testEdgeBlur 5 5 "rgba.r"
                            expectedImage = getDefaultTestPic specPath testName
                        it "in pixel-wise metric" $ do
                            shouldBeCloseTo (specPath++testName) PixelWise actualImage expectedImage 
                        it "in image-wise metric" $ do
                            shouldBeCloseTo (specPath++testName) ImageWise actualImage expectedImage
                    --it "should match previously computed picture on rgba.r channel with dge multiplier 5 and kernel size 5 in every metric" $
                    --    property $ \x -> shouldBeCloseTo x (unsafePerformIO $ testEdgeBlur 5 5 "rgba.r") (unsafePerformIO $ loadImageLuna "./samples/eb_result.png")
                    it "should throw exception on non existing channel" $ do
                        testSaveEdgeBlur 5 5 "rgba.x" `shouldThrow` anyException
                    --it "should be efficent" $ do
                    --    timeout 1000

              --let testName = "dither"
              --  in describe testName $ do
              --      describe "should match previously computed image with param 5" $ do
              --          let actualImage = unsafePerformIO $ testDither 5
              --              expectedImage = getDefaultTestPic specPath testName
              --          it  "in pixel-wise metric" $ do
              --              shouldBeCloseTo (specPath++testName) PixelWise actualImage expectedImage
              --          it "in image-wise metric" $ do
              --              shouldBeCloseTo (specPath++testName) ImageWise actualImage expectedImage


testSaveEdgeBlur kernelSize edgeMultiplier channel = do
    img <- loadImageLuna "./samples/lena.png"
    let a = edgeBlur channel EB.GaussBlur kernelSize edgeMultiplier img
    saveImageLuna "./samples/test_results/x_result.png" a

testEdgeBlur kernelSize edgeMultiplier channel = do
    img <- loadImageLuna "./samples/lena.png"
    let a = edgeBlur channel EB.GaussBlur kernelSize edgeMultiplier img
    return a

testDither a = do
    let mydither = dither M.Clamp floydSteinberg a
    let grad = monosampler $ scale (512 :: Grid (M.Exp Int)) $ circularShape :: DiscreteShader (M.Exp Float)
    result <- M.mutableProcess run mydither $ rasterizer grad
    return result