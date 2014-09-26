{-# LANGUAGE ScopedTypeVariables #-}

module Test.Luna.Typechecker.SubstitutionsSpec (spec) where


import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Substitutions

import Luna.Typechecker.Internal.Logger

import Test.Luna.Typechecker.AST.TypeGen

import Test.Hspec
import Test.QuickCheck

import Data.Either
import Data.List


spec :: Spec
spec = do
  let a  = Tyvar "a" Star
      b  = Tyvar "b" Star
      c  = Tyvar "c" Star

  describe "nullSubst" $
    it "does not affect any type" $ property $
      \t -> apply nullSubst t == (t :: Type)
  describe "(+->)" $ do
    it "yields some result for all reasonably-kinded values" $ property $
      forAll arbitrary    $ \k ->
      forAll (genTyvar k) $ \tvar ->
      forAll (genType k)  $ \ty ->
        (tvar+->ty) `shouldSatisfy` not.null
    describe "enforcing of kind-preserveness" $ do
      it "satisfies that property" $
        pendingWith "waiting for refactor: +-> should be partial  "
      it "passes some basic unit tests" $
        pendingWith "waiting for refactor: +-> should be partial  "
    it "should check for infinite-types" $
      pendingWith "waiting for refactor: +-> should be partial  "
  describe "Substitutions composition (@@)" $
    it "satisfies property: apply (s1 @@ s2) = apply s1 . apply s2" $
      property $
        forAll arbitrary         $ \(t :: Type)  ->
        forAll (genSubst (tv t)) $ \s1 ->
        forAll (genSubst (tv t)) $ \s2 ->
          apply (s1 @@ s2) t == (apply s1 . apply s2) t
  describe "Symmetric substitutions composition (merge)" $
    it "satisfies property: apply (s1 `merge` s2) = apply (s2 `merge` s1) in its domain" $
      property $
        forAll arbitrary         $ \(t :: Type)  ->
        forAll (genSubst (tv t)) $ \s1 ->
        forAll (genSubst (tv t)) $ \s2 ->
          let s12 = evalLogger $ merge s1 s2
              s21 = evalLogger $ merge s2 s1
           in isRight s12 ==> let (Right s12', Right s21') = (s12, s21) in (apply s12' t == apply s21' t)
  describe "class Types t" $ do
    describe "instance Types Type" $ do
      describe "apply :: Subst -> t -> t" $
        it "is" pending
      describe "tv :: t -> [Tyvar]" $
        it "returns variables, in left-to-right order, no duplicates" $ do
          let dot = (TVar b `fn` TVar c) `fn` (TVar a `fn` TVar b) `fn` (TVar a `fn` TVar c)
              dlr = (TVar a `fn` TVar b) `fn` TVar a `fn` TVar b
          tv tInt                 `shouldBe` []
          tv (TVar a)             `shouldBe` [a]
          tv (TVar a `fn` TVar b) `shouldBe` [a, b]
          tv dot                  `shouldBe` [b, c, a]
          tv dlr                  `shouldBe` [a, b]
    describe "instance Types a => Types [a]" $ do
      describe "apply :: Subst -> t -> t" $
        it "is" pending
      describe "tv :: t -> [Tyvar]" $
        it "returns variables, in left-to-right order, no duplicates" $ do
          tv [TVar a, TVar b, TVar c] `shouldBe` [a, b, c]
          tv (subsequences [TVar a, TVar b, TVar c, TVar b]) `shouldBe` [a, b, c]
