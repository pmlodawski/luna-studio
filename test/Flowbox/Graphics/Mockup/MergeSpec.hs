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

                    let actualImage =  (mergeLuna Atop (conicalLuna 1200 1500)) (conicalLuna 300 500) --(loadImageLuna "/home/chris/Lena.png") -- (constantLuna PCVideo (RGBA 0.3 0.4 0.9 0.6))    
                    -- let  expectedImage = getDefaultTestPic specPath testName
                    it "in test" $ do
                        --pending
                        (testSave  actualImage) `shouldReturn` ()
                        -- rightReturnShouldBeCloseTo testPath PixelWise actualImage expectedImage
