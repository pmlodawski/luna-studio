module Flowbox.Graphics.Mockup.TransformSpec where

import Test.Hspec
import Test.QuickCheck
import System.IO.Unsafe

import Flowbox.Graphics.Mockup.Generator
import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.Transform
import Flowbox.Prelude
import Flowbox.Graphics.Color.Color

import TestHelpers

spec :: Spec
spec = do
    let specPath = "./test/Flowbox/Graphics/Mockup/"
        in do 
            let testName = "rotateLuna"
            let testPath = specPath++testName

            describe testName $ do
                describe "Should save ok image" $ do
                    let actualImage = rotateLuna (pi/4) (conicalLuna 100 120)
                    -- let  expectedImage = getDefaultTestPic specPath testName
                    it "in test" $ do
                        pending --testSave actualImage `shouldReturn` ()
                        -- rightReturnShouldBeCloseTo testPath PixelWise actualImage expectedImage