module Test.Hspec.Expectations.LunaTypechecker (
    module OrigExpectation,
    shouldSatisfyLens,
    isResultOf
  ) where


import Flowbox.Prelude

import Test.Hspec.Expectations as OrigExpectation
import Test.HUnit (Assertion)



infix 1 `shouldSatisfyLens`
shouldSatisfyLens :: (Show a) => a -> Getting Bool a Bool -> Expectation
shouldSatisfyLens x l = x `shouldSatisfy` (^. l)

infix 1 `isResultOf`
isResultOf :: (Show a, Eq a) => a -> a -> Assertion
isResultOf = flip shouldBe