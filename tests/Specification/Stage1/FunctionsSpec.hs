module Specification.Stage1.FunctionsSpec (spec) where


import Test.Hspec.LunaTypechecker



spec :: Spec
spec = do
  describe "Basic specification" $ do
    describe "no run-errors" $ do
      it "works for `id`"     $ lunacStepsSuccessFile "tests/Specification/Stage1/Functions/IdentityFunction.luna"
      it "works for `flip`"   $ lunacStepsSuccessFile "tests/Specification/Stage1/Functions/FlipFunction.luna"
      it "works for `const`"  $ lunacStepsSuccessFile "tests/Specification/Stage1/Functions/ConstFunction.luna"
      describe "`const` variations" $ do
        it "works for `const` with wildcard"    $ lunacStepsSuccessFile "tests/Specification/Stage1/Functions/ConstFunctionWildc.luna"
        --it "works for `const` with assignments" $ lunacStepsSuccessFile "tests/Specification/Stage1/Functions/ConstFunctionVars.luna"

  describe "Recursion" $ do
    specify "self-recursion"                $ pending
    specify "declaration order recursion"   $ pending
    specify "recursion not order dependent" $ pending
