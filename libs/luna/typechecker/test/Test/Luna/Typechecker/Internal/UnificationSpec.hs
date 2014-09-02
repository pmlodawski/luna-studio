module Test.Luna.Typechecker.Internal.UnificationSpec (spec) where

import Luna.Typechecker.Internal.AST.Kind       (Kind(..))
import Luna.Typechecker.Internal.AST.Type       (Type(..),Tyvar(..),tUnit,tList,list)

import Luna.Typechecker.Internal.Substitutions  (Subst)
import Luna.Typechecker.Internal.Substitutions  (apply)
import Luna.Typechecker.Internal.Unification    (mgu)

import Control.Exception                        (evaluate)
import Control.Applicative                      ((<$>))

import Test.Hspec

spec :: Spec
spec = do
  describe "mgu" $ do
    it "satisfies property: apply u t1 == apply u t2 for u = mgu t1 t2" $ do
      let t1 = tUnit
          t2 = TVar (Tyvar "a" Star)
          Just u = mgu t1 t2
      apply u t1 `shouldBe` apply u t2
    it "does the kind check" $ do
      let t1 = TVar (Tyvar "a" Star)
          t2 = tList
          u = mgu t1 t2 :: Either String Subst
      evaluate u `shouldThrow` anyErrorCall
    it "does the infinite-type check" $ do
      let t1 = TVar (Tyvar "a" Star)
          t2 = list t1
          u  = mgu t1 t2 :: Either String Subst
      evaluate u `shouldThrow` anyErrorCall
    it "doesn't mind matching equal consts [QC]" $
      pending
    it "errors when types can't be unified" $
      pending
  describe "match" $
    it "some examples for property: apply u t1 == t2 for u = match t1 t2" $ do
      let t1 = TVar (Tyvar "a" Star)
          t2 = tUnit
          u = mgu t1 t2 :: Either String Subst
      evaluate (flip apply t1 <$> u) `shouldReturn` Right t2
  describe "(internals)" $ do
    describe "varBind" $ do
      it "QC: ∀ (Tyvar tv): varBind tv (TVar tv) == nullSubst" $ 
        pendingWith "needs refactoring into second XYZ.Internal module"
      it "QC: ∀ (Tyvar tv, Type t): u `elem` tv t  ===>  varBind tv t == ⊥" $
        pending
      it "QC: ∀ (Tyvar tv, Type t): kind tv /= kind t  ===>  varBind tv t == ⊥" $
        pending