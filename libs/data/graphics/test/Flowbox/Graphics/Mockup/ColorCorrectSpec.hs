module Flowbox.Graphics.Mockup.ColorCorrectSpec where

import Test.Hspec
import Test.QuickCheck

import Flowbox.Graphics.Mockup.ColorCorrect
import Flowbox.Graphics.Mockup.Generator
import Flowbox.Graphics.Color.Companding
import Flowbox.Prelude
import TestHelpers

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
                        testSave actualImage `shouldReturn` ()
                        -- rightReturnShouldBeCloseTo testPath PixelWise actualImage expectedImage
