module Flowbox.Graphics.Mockup.MergeSpec where

import Test.Hspec
import Test.QuickCheck

import Flowbox.Graphics.Mockup.Merge
import Flowbox.Graphics.Mockup.Generator
import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Color.Color
import Flowbox.Prelude
import Control.Monad
import TestHelpers

spec :: Spec
spec = do
    let specPath = "./test/Flowbox/Graphics/Mockup/"
        in do 
            let testName = "mergeLuna"
            let testPath = specPath++testName

            describe testName $ do
                describe "Should save ok image" $ do

                    let actualImage = liftM( mergeLuna Over (conicalLuna 1200 1200) ) (loadImageLuna "./test/samples/lena.png") -- (constantLuna PCVideo (RGBA 0.3 0.4 0.9 0.6))    
                    -- let  expectedImage = getDefaultTestPic specPath testName
                    it "in test" $ do
                        pending
                        (testSave =<< actualImage) `shouldReturn` ()
                        -- rightReturnShouldBeCloseTo testPath PixelWise actualImage expectedImage

            describe "should match reference image" $ do
                let actualImage = liftM( mergeLuna Over (conicalLuna 1200 1200) ) (loadImageLuna "./test/samples/lena.png")
                    expectedImage = getDefaultTestPic specPath testName
                it "in pixel-wise metric" $ do
                    returnShouldBeCloseTo testPath PixelWise actualImage expectedImage
                it "in image-wise metric" $ do
                    returnShouldBeCloseTo testPath ImageWise actualImage expectedImage
