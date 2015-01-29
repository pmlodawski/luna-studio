module Specification.Stage1.FunctionsSpec (spec) where

import Control.Applicative
import Control.Lens
import Data.Either
import Test.Hspec.LunaTypechecker



spec :: Spec
spec = do
  describe "Basic specification" $ do
    it "does not yield any errors for `id`" $ do
      tmp <- lunaCompilerStepsFile "tests/Specification/Stage1/Functions/IdentityFunction.luna"
      print tmp
      -- lunaCompilerStepsSuccess

    it "does not yield any errors for `const`" $ do
      tmp <- lunaCompilerStepsFile "tests/Specification/Stage1/Functions/ConstFunction.luna"
      print tmp
      -- lunaCompilerStepsSuccess

    it "does not yield any errors for `flip`" $ do
      tmp <- lunaCompilerStepsFile "tests/Specification/Stage1/Functions/FlipFunction.luna"
      print tmp
      -- lunaCompilerStepsSuccess