module Flowbox.Graphics.Mockup.MergeSpec where

import Test.Hspec
import Test.QuickCheck

import Flowbox.Graphics.Mockup.Merge
import Flowbox.Graphics.Mockup.Generator
import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.Matte
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
            let modes = [  Atop
                           , Average Custom
                           , ColorBurn Custom
                           , ColorDodge Custom
                           , ConjointOver
                           , Copy Custom
                           , Difference Custom
                           , DisjointOver
                           , DivideBySource Custom
                           , DivideByDestination Custom
                           , Exclusion Custom
                           , From Custom
                           , Geometric Custom
                           , HardLight Custom
                           , Hypot Custom
                           , In
                           , MergeMask
                           , MergeMatte
                           , MergeMax Custom
                           , MergeMin Custom
                           , Minus Custom
                           , Multiply Custom
                           , Out
                           , Over
                           , Overlay Custom
                           , Plus Custom
                           , Screen Custom
                           , SoftLight Custom
                           , SoftLightPegtop Custom
                           , SoftLightIllusions Custom
                           , SoftLightPhotoshop Custom
                           , Stencil
                           , Under
                           , XOR ]

            describe testName $ do
                describe "Should save ok images" $ do -- map ( \x ->

                    let actualImages = map (\x -> liftM3 (mergeLuna x) (loadImageLuna "./test/samples/edge/desert.png")  (loadImageLuna "/home/chris/globe.png") (return matte) ) modes -- (constantLuna PCVideo (RGBA 0.3 0.4 0.9 0.6))    
                        matte = imageMatteLuna (constantLuna (CustomFormat 200 200) (RGBA 1 1 1 1)) "rgba.r" 
                    --let actualImage = liftM( mergeLuna Over (conicalLuna 1200 1200) ) (loadImageLuna "./test/samples/lena.png") Nothing
                    --let actualImage = mergeLuna Over (conicalLuna 1000 1200) (constantLuna PCVideo (RGBA 0.3 0.4 0.5 0.6)) Nothing
                    -- let  expectedImage = getDefaultTestPic specPath testName
                    it "in test" $ do
                        pending
                        (zipWithM_ (\x y -> ((nameSave (show x)) =<< y)) modes actualImages) `shouldReturn` ()
                        -- rightReturnShouldBeCloseTo testPath PixelWise actualImage expectedImage
            
            let actualImage = liftM2 (mergeLuna Over (conicalLuna 1200 1200)) (loadImageLuna "./test/samples/lena.png") (return Nothing)        
            defaultReferenceTestM testName specPath actualImage
            --describe "should match reference image" $ do
            --    let actualImage = liftM2 (mergeLuna Over (conicalLuna 1200 1200)) (loadImageLuna "./test/samples/lena.png") (return Nothing)
            --        expectedImage = getDefaultTestPic specPath testName
            --    it "in pixel-wise metric" $ do
            --        returnShouldBeCloseTo testPath PixelWise actualImage expectedImage
            --    it "in image-wise metric" $ do
            --        returnShouldBeCloseTo testPath ImageWise actualImage expectedImage

nameSave name image = do
    saveImageLuna ("./test/samples/mergeResults/"++name++".png") image
    return ()