module Flowbox.Graphics.Mockup.GeneratorSpec where

import Test.Hspec
import Test.QuickCheck
import System.IO.Unsafe

import Flowbox.Graphics.Mockup.Generator
import Flowbox.Graphics.Mockup.Basic
import Flowbox.Prelude
import Flowbox.Graphics.Color.Color
import Flowbox.Graphics.Composition.Generator.Raster

import TestHelpers


spec :: Spec
spec = do
	let specPath = "./test/Flowbox/Graphics/Mockup/"
		in do 
		  	let testName = "constantLuna"
			  	--testPath = specPath++testName

			describe testName $ do
				describe "should match reference image" $ do
					let actualImage = constantLuna PCVideo (RGBA 0.3 0.4 0.5 0.6)
						--expectedImage = getDefaultTestPic specPath testName
					it "in pixel-wise metric" $ do
						 shouldBeCloseTo (specPath++testName) PixelWise actualImage (unsafePerformIO $ getDefaultTestPic specPath testName)






testSaveConstant image = do
    saveImageLuna "./test/samples/x_result.png" image
    return ()