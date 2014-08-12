module Flowbox.Test.QuickCheck (
    module Flowbox.Test.QuickCheck,
    module X
) where

import           Test.QuickCheck as X
import           Test.Hspec      as X hiding (shouldBe)
import qualified Test.Hspec      as Hspec
import           Data.Typeable



shouldBeStrict :: (Show a, Eq a) => a -> a -> Expectation
shouldBeStrict = Hspec.shouldBe

shouldHaveType :: (Typeable a, Typeable b, Show b) => a -> b -> Expectation
shouldHaveType a b = typeOf a `shouldBeStrict` typeOf b

shouldBe :: (Typeable a, Typeable b, Show b, Eq b) => a -> b -> Expectation
shouldBe a b = maybe (shouldHaveType a b) (\b' -> b' `shouldBeStrict` b) (cast a)