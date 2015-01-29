module EdgeBlurSpec where

import Test.Hspec
import Test.QuickCheck
import Flowbox.Graphics.Composition.EdgeBlur

import Flowbox.Prelude as P
import Control.Applicative

import Control.Exception.Base (evaluate)
import System.Timeout (timeout)

import Data.Maybe


spec :: Spec
spec = do 
    describe "eee func" $ do
        it "should increment arument" $
            property $ \x -> 
                eee x `shouldSatisfy` (==x+(1 ::Int))

    describe "magical function" $ do
        it "should do magic" $ do
            pendingWith "waits for inventing magic"

    describe "mixImages" $ do
        describe "should for zero mask return second" $ do
            it "for Maybe" $ do
                let mask   = Just 0
                    first  = Just 1
                    second = Just 2 
                mixImages mask first second `shouldBe` second
            it "for ZipList" $ do
                let mask   = ZipList [0,0.1]
                    first  = ZipList [1,2]
                    second = ZipList [4,5]
                mixImages mask first second `shouldSatisfy` (==second)
        
        it "should for one mask return first" $ do
            let mask   = Just 1
                first  = Just 1
                second = Just 2 
            mixImages mask first second `shouldBe` first
        
        it "should for 0.5 mask return average" $
            property $ \x y -> 
                let mask      = Just 0.5
                    firstNum  = x :: Float
                    secondNum = y :: Float
                    avg       = (firstNum + secondNum)/2
                    first     = Just firstNum
                    second    = Just secondNum
                in mixImages mask first second `shouldBe` (Just avg)

        it "should be efficient" $ do
            pending
            --let big  = ZipList [0.5::Float | _<-[1..1000000]]
            --timeout 10 (evaluate $ mixImages big big big) `shouldReturn` Just big -- isNothing???
            --timeout 10000 (evaluate $ sum [1..10000] `mod` 10) `shouldReturn` Just 0





