module Test.Hspec.Expectations.LunaTypechecker (
    module OrigExpectation,
    shouldSatisfyLens
  ) where


import Test.Hspec.Expectations as OrigExpectation
import Control.Lens


shouldSatisfyLens :: (Show a) => a -> Getting Bool a Bool -> Expectation
shouldSatisfyLens x l = x `shouldSatisfy` (^. l)