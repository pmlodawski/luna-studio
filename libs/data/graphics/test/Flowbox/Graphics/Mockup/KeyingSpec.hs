module Flowbox.Graphics.Mockup.KeyingSpec where

import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad
import           Flowbox.Graphics.Color.Color
import           Flowbox.Graphics.Composition.Keying
import           Flowbox.Graphics.Mockup.Basic
import           Flowbox.Graphics.Mockup.Generator
import           Flowbox.Graphics.Mockup.Keying
import           Flowbox.Prelude

import           TestHelpers


spec :: Spec
spec = do
    let specPath = "./test/Flowbox/Graphics/Mockup/"
        in do
            let testName = "differenceKeyerLuna"
            let testPath = specPath++testName

            describe testName $ do
                describe "should save proper image" $ do
                    let actualImage = differenceKeyerLuna 0.2 2.5 True (constantLuna HD (RGBA 0.9 0.4 0.5 0.9)) (conicalLuna 200 400)
                    let expectedImage = undefined --getDefaultTestPic specPath testName
                    it "to samles/x_result" $ do
                        pending
                        testSave actualImage `shouldReturn` ()
                        --shouldBeCloseTo testPath PixelWise actualImage (unsafePerformIO expectedImage)
                    it "in image-wise metric" $ do
                        pending
                        --shouldBeCloseTo testPath ImageWise actualImage (unsafePerformIO expectedImage)
                    it "in size-wise metric" $ do
                        pending
                        --shouldBeCloseTo testPath SizeWise actualImage (unsafePerformIO expectedImage)

            do
                let testName = "keyerLuna"
                let actualImage = liftM (keyerLuna Greenscreen (KeyerThresholds 0 1 1 1) ) $ loadImageLuna "/home/chris/key.jpg"
                defaultReferenceTestM testName specPath actualImage

                --describe testName $ do

                --    -- let expectedImage = getDefaultTestPic specPath testName
                --    it "should save img" $ do
                --        (testSave=<<actualImage) `shouldReturn` ()
                --    describe "should match reference image" $ do
                --        it "in pixel-wise metric" $ do
                --            pending
                --            shouldBeCloseTo testPath PixelWise actualImage (unsafePerformIO expectedImage)
