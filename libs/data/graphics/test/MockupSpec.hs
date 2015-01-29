module MockupSpec where

import Test.Hspec
import Test.QuickCheck
import Flowbox.Graphics.Mockup as M
import Flowbox.Graphics.Composition.EdgeBlur as EB

import Flowbox.Prelude as P


spec :: Spec
spec = do 
	describe "edgeBlur" $ do
		it "should throw exception on non existing channel" $ do
			testEdgeBlur 1 1 "rgba.x" `shouldThrow` anyErrorCall


testEdgeBlur kernelSize edgeMultiplier channel = do
    img <- loadImageLuna "/home/chris/globe.png"
    let a = edgeBlur channel EB.GaussBlur kernelSize edgeMultiplier img
    saveImageLuna "/home/chris/Luna_result.png" a