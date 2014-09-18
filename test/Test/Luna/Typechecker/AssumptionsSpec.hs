module Test.Luna.Typechecker.AssumptionsSpec (spec) where
import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.Type
import Luna.Typechecker.Assumptions
import Luna.Typechecker.Typeclasses

import Test.Hspec
import Control.Exception

spec :: Spec
spec =
  describe "find" $ do
    it "can fail" $ do
      let res = find "a" [] :: Either String Scheme
      evaluate res `shouldThrow` anyErrorCall
    it "works for singletons" $ do
      let res = find "a" ["a" :>: sch]
          sch = Forall [] ([] :=> (TVar $ Tyvar "a" Star))
      evaluate res `shouldReturn` Just sch
    it "recurses" $ do
      let res = find "a" ["b" :>: sch2, "a" :>: sch1]
          sch1 = Forall [] ([] :=> (TVar $ Tyvar "a" Star))
          sch2 = Forall [] ([] :=> (TVar $ Tyvar "b" Star))
      evaluate res `shouldReturn` Just sch1
