module Flowbox.Graphics.Mockup.GeneratorSpec where

import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck

import           Flowbox.Graphics.Color.Color
import           Flowbox.Graphics.Mockup.Basic
import           Flowbox.Graphics.Mockup.Generator
import           Flowbox.Prelude

import           TestHelpers


spec :: Spec
spec = do
    let specPath = "./test/Flowbox/Graphics/Mockup/"
        in do
            let testName = "constantLuna"
            let testPath = specPath++testName

            describe testName $ do
                describe "Should match reference image" $ do
                    let actualImage = constantLuna PCVideo (RGBA 0.3 0.4 0.5 0.6)
                    let expectedImage = getDefaultTestPic specPath testName
                    it "in pixel-wise metric" $ do
                        rightReturnShouldBeCloseTo testPath PixelWise actualImage expectedImage
                    it "in image-wise metric" $ do
                        rightReturnShouldBeCloseTo testPath ImageWise actualImage expectedImage
                    it "in size-wise metric" $ do
                        rightReturnShouldBeCloseTo testPath SizeWise actualImage expectedImage

            do
                let testName = "conicalLuna"
                let testPath = specPath++testName

                describe testName $ do
                    let actualImage = conicalLuna 100 120
                    let expectedImage = getDefaultTestPic specPath testName
                    --it "should save img" $ do
                    --  testSave actualImage `shouldReturn` ()
                    describe "should match reference image" $ do
                        it "in pixel-wise metric" $ do
                            shouldBeCloseTo testPath PixelWise actualImage (unsafePerformIO expectedImage)

