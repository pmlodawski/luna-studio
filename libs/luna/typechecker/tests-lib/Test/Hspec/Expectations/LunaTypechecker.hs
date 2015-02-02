module Test.Hspec.Expectations.LunaTypechecker (
    module OrigExpectation,
    shouldSatisfyLens
  ) where


import Flowbox.Prelude

import Test.Hspec.Expectations as OrigExpectation



shouldSatisfyLens :: (Show a) => a -> Getting Bool a Bool -> Expectation
shouldSatisfyLens x l = x `shouldSatisfy` (^. l)