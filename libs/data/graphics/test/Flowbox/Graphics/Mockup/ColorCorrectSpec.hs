module Flowbox.Graphics.Mockup.ColorCorrectSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Flowbox.Graphics.Color.Color
import           Flowbox.Graphics.Color.Companding
import           Flowbox.Graphics.Mockup.Basic
import           Flowbox.Graphics.Mockup.ColorCorrect
import           Flowbox.Graphics.Mockup.Generator
import           Flowbox.Prelude
import           System.IO.Unsafe
import           TestHelpers

spec :: Spec
spec = do
    let specPath = "./test/Flowbox/Graphics/Mockup/"
        in do
            let testName = "gammaToLinearLuna"
            let testPath = specPath++testName

            describe testName $ do
                describe "Should save ok image" $ do
                    let actualImage = gammaToLinearLuna REDLog (conicalLuna 1000 1200)
                    -- let  expectedImage = getDefaultTestPic specPath testName
                    it "in test" $ do
                        pending
                        --testSave actualImage `shouldReturn` ()
                        -- rightReturnShouldBeCloseTo testPath PixelWise actualImage expectedImage

            let testName = "saturateLuna"
                testPath = specPath++testName

            describe testName $ do
                describe "should save ok image" $ do
                    --let actualImage = saturateLuna (RGB 0 1 0) (unsafePerformIO $ loadImageLuna "/home/chris/globe.png")

                    it "in test" $ do
                        pending
                        --testSave actualImage `shouldReturn` ()

