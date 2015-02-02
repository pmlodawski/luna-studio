module Specification.Stage1.FunctionsSpec (spec) where


import Test.Hspec.LunaTypechecker



spec :: Spec
spec = do
  describe "Basic specification" $ do
    it "does not yield any errors for `id`" $    lunaCompilerStepsSuccess =<< lunaCompilerStepsFile "tests/Specification/Stage1/Functions/IdentityFunction.luna"
    it "does not yield any errors for `const`" $ lunaCompilerStepsSuccess =<< lunaCompilerStepsFile "tests/Specification/Stage1/Functions/ConstFunction.luna"
    it "does not yield any errors for `flip`" $  lunaCompilerStepsSuccess =<< lunaCompilerStepsFile "tests/Specification/Stage1/Functions/FlipFunction.luna"