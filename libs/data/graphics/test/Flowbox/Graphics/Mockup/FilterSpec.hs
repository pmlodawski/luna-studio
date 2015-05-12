module Flowbox.Graphics.Mockup.FilterSpec where

import Test.Hspec
import Test.QuickCheck
import Flowbox.Graphics.Mockup.Basic as M
import Flowbox.Graphics.Mockup.Filter as M
import Flowbox.Graphics.Composition.EdgeBlur as EB
import qualified Flowbox.Math.Matrix as M
import System.IO.Unsafe
import Control.Monad
import qualified Flowbox.Graphics.Image.Image   as Img
import qualified Flowbox.Graphics.Image.Channel as Chan
import qualified Flowbox.Graphics.Image.View    as View
import Flowbox.Graphics.Utils.Utils (clamp') 

import Flowbox.Prelude as P

import TestHelpers


spec :: Spec
spec = do
    let specPath = "./test/Flowbox/Graphics/Mockup/"
        in do 
          let testName = "edgeBlur"
              testPath = specPath++testName
                in describe testName $ do 
                    describe "should match previously computed picture on rgba.r channel with edge multiplier 5 and kernel size 5" $ do
                        let actualImage = testEdgeBlur 5 5 "rgba.r"
                            expectedImage = getDefaultTestPic specPath testName
                        it "in pixel-wise metric" $ do
                            returnShouldBeCloseTo testPath PixelWise actualImage expectedImage 
                        it "in image-wise metric" $ do
                            returnShouldBeCloseTo testPath ImageWise actualImage expectedImage
                    --it "should match previously computed picture on rgba.r channel with dge multiplier 5 and kernel size 5 in every metric" $
                    --    property $ \x -> shouldBeCloseTo x (unsafePerformIO $ testEdgeBlur 5 5 "rgba.r") (unsafePerformIO $ loadImageLuna "./samples/eb_result.png")
                    it "should throw exception on non existing channel" $ do
                        testSaveEdgeBlur 5 5 "rgba.x" `shouldThrow` anyException
                    it "should be efficent" $ do
                        pending

          let testName = "edgeDetectLuna"
              testPath = specPath++testName
                in describe testName $ do
                    describe "should save ok image" $ do
                        let actualImage = liftM (edgeDetectLuna (Sobel Horizontal)) $ loadImageLuna "./test/samples/lena.png"
                        it "in test" $ do
                            pending
                            (testSave =<< actualImage) `shouldReturn` () --(testSave =<< actualImage)
                    describe "should match reference image" $ do
                        let actualImage = liftM (Img.map (View.map (Chan.unsafeMap (Chan.FunFloat (clamp' 0 1))))) $ liftM (edgeDetectLuna (Sobel Horizontal)) $ loadImageLuna "./test/samples/lena.png"
                            expectedImage = getDefaultTestPic specPath testName
                        it "in pixel-wise metric" $ do
                            returnShouldBeCloseTo testPath PixelWise actualImage expectedImage
                        it "in image-wise metric" $ do
                            returnShouldBeCloseTo testPath ImageWise actualImage expectedImage

          let testName = "blurLuna"
              actualImage = liftM (blurLuna 20) $ loadImageLuna "./test/samples/lena.png"
                in 
                    --defaultReferenceSaveM testName specPath actualImage
                    defaultReferenceTestM testName specPath actualImage

          let testName = "closeLuna"
              actualImage = liftM (closeLuna 5) $ loadImageLuna "./test/samples/lena.png"
                in 
                    --defaultReferenceSaveM testName specPath actualImage
                    defaultReferenceTestM testName specPath actualImage

          let testName = "histEqLuna"
              actualImage = liftM (histEqLuna 10) $ loadImageLuna "./test/samples/lena.png"
                in 
                    --defaultReferenceSaveM testName specPath actualImage
                    defaultReferenceTestM testName specPath actualImage


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
    img <- loadImageLuna "./test/samples/lena.png"
    let a = edgeBlurLuna channel EB.GaussBlur kernelSize edgeMultiplier img
    saveImageLuna "./libs/data/graphics/test/samples/x_result.png" a

testEdgeBlur kernelSize edgeMultiplier channel = do
    img <- loadImageLuna "./test/samples/lena.png"
    let a = edgeBlurLuna channel EB.GaussBlur kernelSize edgeMultiplier img
    return a

--testDither a = do
--    let mydither = dither M.Clamp floydSteinberg a
--    let grad = monosampler $ scale (512 :: Grid (M.Exp Int)) $ circularShape :: DiscreteShader (M.Exp Float)
--    result <- M.mutableProcess run mydither $ rasterizer grad
--    return result
