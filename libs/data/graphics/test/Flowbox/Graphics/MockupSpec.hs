module Flowbox.Graphics.MockupSpec where

import Test.Hspec
import Test.QuickCheck
import Flowbox.Graphics.Mockup as M
import Flowbox.Graphics.Composition.EdgeBlur as EB
import System.IO.Unsafe

import Flowbox.Prelude as P

import TestHelpers


spec :: Spec
spec = do 
	describe "edgeBlur" $ do 
        it "should match previously computed picture on rgba.r channel with dge multiplier 5 and kernel size 5 in pixel-wise metric" $ do
            shouldBeCloseTo PixelWise (unsafePerformIO $ testEdgeBlur 5 5 "rgba.r") (unsafePerformIO $ loadImageLuna "./samples/eb_result.png")
        it "should match previously computed picture on rgba.r channel with dge multiplier 5 and kernel size 5 in image-wise metric" $ do
            shouldBeCloseTo ImageWise (unsafePerformIO $ testEdgeBlur 1 5 "rgba.r") (unsafePerformIO $ loadImageLuna "./samples/eb_result.png")
        --it "should match previously computed picture on rgba.r channel with dge multiplier 5 and kernel size 5 in every metric" $
        --    property $ \x -> shouldBeCloseTo x (unsafePerformIO $ testEdgeBlur 5 5 "rgba.r") (unsafePerformIO $ loadImageLuna "./samples/eb_result.png")
        it "should throw exception on non existing channel" $ do
            testSaveEdgeBlur 1 1 "rgba.x" `shouldThrow` anyException 

        

testSaveEdgeBlur kernelSize edgeMultiplier channel = do
    img <- loadImageLuna "./samples/lena_small_png.png"
    let a = edgeBlur channel EB.GaussBlur kernelSize edgeMultiplier img
    saveImageLuna "./samples/x_result.png" a

testEdgeBlur kernelSize edgeMultiplier channel = do
    img <- loadImageLuna "./samples/lena_small_png.png"
    let a = edgeBlur channel EB.GaussBlur kernelSize edgeMultiplier img
    return a


