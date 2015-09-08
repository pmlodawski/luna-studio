module Flowbox.Graphics.Mockup.TransformSpec where

import Test.Hspec
import Test.QuickCheck

import Flowbox.Graphics.Mockup.Generator
import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.Transform
import Flowbox.Prelude
import Flowbox.Graphics.Color.Color
import Flowbox.Geom2D.Rectangle
import Math.Coordinate.Cartesian
import Control.Monad
import Linear (V2 (..))
import Flowbox.Graphics.Mockup.Matte


import TestHelpers

spec :: Spec
spec = do
    let specPath = "./test/Flowbox/Graphics/Mockup/"
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
        in defaultReferenceTest "rotateLuna" "./test/Flowbox/Graphics/Mockup/" actualImage


    let testName = "cropLuna"
        actualImage = liftM (cropLuna (Rect 10 20 310 420) False True) $ loadImageLuna "./test/samples/lena.png"
    --defaultReferenceSaveM testName specPath actualImage
        in defaultReferenceTestM testName specPath actualImage

    let testName = "translateLuna"
        actualImage = liftM (translateLuna (V2 100 200) True Nothing) $ loadImageLuna "./test/samples/lena.png"
    --defaultReferenceSaveM testName specPath actualImage
        in defaultReferenceTestM testName specPath actualImage

    let matte = imageMatteLuna (constantLuna (CustomFormat 200 200) (RGBA 1 0 0 0)) "rgba.r" 

    let testName = "masked_translateLuna"    
        actualImage = liftM (translateLuna (V2 40 60) True matte) $ loadImageLuna "./test/samples/lena.png"
    --defaultReferenceSaveM testName specPath actualImage
        in defaultReferenceTestM testName specPath actualImage

    let testName = "rotateAtLuna"
        actualImage = liftM (rotateAtLuna (Point2 100 400) (pi/4) True Nothing) $ loadImageLuna "./test/samples/lena.png"
    --defaultReferenceSaveM testName specPath actualImage
        in defaultReferenceTestM testName specPath actualImage

    let testName = "masked_rotateAtLuna"
        actualImage = liftM (rotateAtLuna (Point2 100 400) (pi/8) True matte) $ loadImageLuna "./test/samples/lena.png"
    --defaultReferenceSaveM testName specPath actualImage
        in defaultReferenceTestM testName specPath actualImage

    let testName = "scaleAtLuna"
        actualImage = liftM (scaleAtLuna (Point2 100 400) (V2 0.8 2) True Nothing) $ loadImageLuna "./test/samples/lena.png"
    --defaultReferenceSaveM testName specPath actualImage
        in defaultReferenceTestM testName specPath actualImage

    let testName = "masked_scaleAtLuna"
        actualImage = liftM (scaleAtLuna (Point2 100 400) (V2 0.8 2) True matte) $ loadImageLuna "./test/samples/lena.png"
    --defaultReferenceSaveM testName specPath actualImage
        in defaultReferenceTestM testName specPath actualImage