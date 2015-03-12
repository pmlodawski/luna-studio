module Flowbox.Graphics.Mockup.TransformSpec where

import Test.Hspec
import Test.QuickCheck

import Flowbox.Graphics.Mockup.Generator
import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.Transform
import Flowbox.Prelude
import Flowbox.Graphics.Color.Color
import Math.Coordinate.Cartesian

import TestHelpers

spec :: Spec
spec = do
    --let specPath = "./test/Flowbox/Graphics/Mockup/"
    --    in do 
    --        let testName = "rotateLuna"
    --        let testPath = specPath++testName

    --        describe testName $ do
    --            describe "Should save ok image" $ do
    --                let actualImage = rotateAtLuna (Point2 0 0) (-pi/8) False Nothing (conicalLuna 100 120)
    --                --let actualImage = rotateAtLuna (pi/4) (conicalLuna 100 120)
    --                -- let  expectedImage = getDefaultTestPic specPath testName
    --                it "in test" $ do
    --                    pending
    --                    testSave actualImage `shouldReturn` ()
    --            --describe "should match reference image" $ do
    --            --    let actualImage = rotateAtLuna (Point2 0 0) (-pi/8) False Nothing (conicalLuna 100 120)
    --            --        expectedImage = getDefaultTestPic specPath testName
    --            --    it "in pixel-wise metric" $ do
    --            --        rightReturnShouldBeCloseTo testPath PixelWise actualImage expectedImage
    --            --    it "in image-wise metric" $ do
    --            --        rightReturnShouldBeCloseTo testPath ImageWise actualImage expectedImage
    --        describe testName $ do
                let actualImage = rotateAtLuna (Point2 0 0) (-pi/8) False Nothing (conicalLuna 100 120)
                --defaultReferenceTest testName specPath actualImage
                defaultReferenceTest "rotateLuna" "./test/Flowbox/Graphics/Mockup/" actualImage
