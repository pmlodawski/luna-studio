module Specification.Stage1.FunctionsSpec (spec) where

import Control.Applicative
import Control.Lens
import Data.Either
import Test.Hspec.LunaTypechecker



spec :: Spec
spec = do
  describe "Basic specification" $ do
    it "does not yield any errors for `id`" $ do
      lunaCompilerStepsSuccess =<< lunaCompilerStepsFile "tests/Specification/Stage1/Functions/IdentityFunction.luna"

    it "does not yield any errors for `const`" $ do
      lunaCompilerStepsSuccess =<< lunaCompilerStepsFile "tests/Specification/Stage1/Functions/ConstFunction.luna"

    it "does not yield any errors for `flip`" $ do
      lunaCompilerStepsSuccess =<< lunaCompilerStepsFile "tests/Specification/Stage1/Functions/FlipFunction.luna"