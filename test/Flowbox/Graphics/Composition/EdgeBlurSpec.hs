

module Flowbox.Graphics.Composition.EdgeBlurSpec where

import TestHelpers
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Flowbox.Graphics.Composition.EdgeBlur

import Debug.Trace
import System.IO.Unsafe
 -- import System.Console.ANSI


import Flowbox.Prelude as P
import Control.Applicative

import Control.Exception.Base (evaluate)
import System.Timeout (timeout)

import Data.Maybe


spec :: Spec
spec = do 
    describe "eee func" $ do
        it "should increment any argument" $
            property $ \x -> 
                eee x `shouldSatisfy` (==x+(1 ::Int))

    describe "magical function" $ do
        it "should do magic" $ do
            pendingWith "waits for inventing magic"

    describe "mixImages" $ do
        describe "should return second" $ do
            context "for zero mask" $ do
                it "for Maybe" $ do
                    let mask   = Just 0
                        first  = Just 1
                        second = Just 2 
                    mixImages mask first second `shouldBe` second
                it "for ZipList" $ do
                    pending
                    --let mask   = ZipList [0,0]
                    --    first  = ZipList [1,2]
                    --    second = ZipList [4,5]
                    --mixImages mask first second `shouldSatisfy` (closeTo second)
        
        it "should for one mask return first" $ do
            let mask   = Just 1
                first  = Just 1
                second = Just 2 
            mixImages mask first second `shouldBe` first
        
        it "should for 0.5 mask return average" $
            property $ \x y -> 
                let mask      = Just 0.50
                    firstNum  = x :: Float
                    secondNum = y :: Float
                    avg       = (firstNum + secondNum)/2
                    first     = Just firstNum
                    second    = Just secondNum
                in shouldBeCloseTo "mixImages" (Close (0.5::Float)) (mixImages mask first second) (Just avg)

        it "should be efficient" $ do
            pending
            let big  = ZipList [0.5::Float | _<-[1..10000000]]
            timeout 1000 (evaluate $ mixImages big big big) `shouldReturn` Just big -- isNothing???
            --timeout 10000 (evaluate $ sum [1..10000] `mod` 10) `shouldReturn` Just 0



--closeTo :: (Eq a, Show a) => a -> a -> Bool
--closeTo a b = 
--    if a==b
--        then True
--        else unsafePerformIO $ do
--            setSGR [SetColor Background Dull Red]
--            print a
--            print b
--            setSGR []
--            return False

        --trace ("expected " ++ show a ++ "\ngot " ++ show b) False




-- infix 1 `shouldBeCloseTo`, `closeEnough`

