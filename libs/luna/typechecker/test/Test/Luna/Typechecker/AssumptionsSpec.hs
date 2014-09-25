module Test.Luna.Typechecker.AssumptionsSpec (spec) where


import Luna.Typechecker.Typeclasses
import Luna.Typechecker.Assumptions

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Data.Either

import Test.Hspec


spec :: Spec
spec =
  describe "find" $ do
    it "can fail" $ do
      let res = evalLogger $ find "a" []
      res `shouldSatisfy` isLeft
    it "works for singletons" $ do
      let res = evalLogger $ find "a" ["a" :>: sch]
          sch = Forall [] ([] :=> (TVar $ Tyvar "a" Star))
      res `shouldBe` Right sch
    it "recurses" $ do
      let res = evalLogger $ find "a" ["b" :>: sch2, "a" :>: sch1]
          sch1 = Forall [] ([] :=> (TVar $ Tyvar "a" Star))
          sch2 = Forall [] ([] :=> (TVar $ Tyvar "b" Star))
      res `shouldBe` Right sch1
