module Test.Luna.Typechecker.AssumptionsSpec (spec) where


import Luna.Typechecker.Typeclasses
import Luna.Typechecker.Assumptions

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Data.Either

import Test.Hspec


spec :: Spec
spec =
  describe "find" $ do
    it "can fail" $ do
      let res = evalLogger $ find (TID "a") []
      res `shouldSatisfy` isLeft
    it "works for singletons" $ do
      let res = evalLogger $ find (TID "a") [TID "a" :>: sch]
          sch = Forall [] ([] :=> (TVar $ Tyvar (TID "a") Star))
      res `shouldBe` Right sch
    it "recurses" $ do
      let res = evalLogger $ find (TID "a") [TID "b" :>: sch2, TID "a" :>: sch1]
          sch1 = Forall [] ([] :=> (TVar $ Tyvar (TID "a") Star))
          sch2 = Forall [] ([] :=> (TVar $ Tyvar (TID "b") Star))
      res `shouldBe` Right sch1
