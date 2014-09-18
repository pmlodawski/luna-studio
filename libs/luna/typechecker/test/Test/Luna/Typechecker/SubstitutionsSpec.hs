{-# LANGUAGE ScopedTypeVariables #-}

module Test.Luna.Typechecker.SubstitutionsSpec (spec) where

import qualified Luna.Typechecker.AST.Kind         as Knd
import qualified Luna.Typechecker.AST.Type         as Ty
import           Luna.Typechecker.AST.Type         (Type(TVar), fn)

import           Luna.Typechecker.Substitutions

import           Test.Luna.Typechecker.AST.TypeGen

import           Test.Hspec
import           Test.QuickCheck
--import           Control.Exception                          (evaluate)

import           Data.List                                  (subsequences)
import           Data.Maybe                                 (fromJust, isJust)

spec :: Spec
spec = do
  let a  = Ty.Tyvar "a" Knd.Star
      b  = Ty.Tyvar "b" Knd.Star
      c  = Ty.Tyvar "c" Knd.Star

  describe "nullSubst" $
    it "does not affect any type" $ property $
      \t -> apply nullSubst t == (t :: Ty.Type)
  describe "(+->)" $ do
    it "yields some result for all reasonably-kinded values" $ property $
      forAll arbitrary    $ \k ->
      forAll (genTyvar k) $ \tvar ->
      forAll (genType k)  $ \ty ->
        (tvar+->ty) `shouldSatisfy` not.null
    describe "enforcing of kind-preserveness" $ do
      it "satisfies that property" $
        pendingWith "waiting for refactor: +-> should be partial  "
        --property $
        --forAll arbitrary     $ \k1 ->
        --forAll arbitrary     $ \k2 ->
        --forAll (genTyvar k1) $ \tv ->
        --forAll (genType k2)  $ \ty ->
        --  (k1/=k2) ==> ((tv+->ty) `shouldBe` [])
      it "passes some basic unit tests" $
        pendingWith "waiting for refactor: +-> should be partial  "
        --let a = Ty.Tyvar "a" Knd.Star
        --    b = Ty.Tyvar "b" (Knd.Kfun Knd.Star Knd.Star)
        --a           +-> Ty.tInt     `shouldSatisfy` not . null
        --evaluate (a +-> Ty.tList)   `shouldThrow`   anyErrorCall
        --b           +-> Ty.tList    `shouldSatisfy` not . null
    it "should check for infinite-types" $
      pendingWith "waiting for refactor: +-> should be partial  "
      --let a = Ty.Tyvar "a" Knd.Star
      --evaluate (a +-> Ty.list (Ty.TVar a)) `shouldThrow` anyErrorCall
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
          let s12 = merge s1 s2
              s21 = merge s2 s1
           in isJust s12 ==> apply (fromJust s12) t == apply (fromJust s21) t
  describe "class Types t" $ do
    describe "instance Types Type" $ do
      describe "apply :: Subst -> t -> t" $
        it "is" pending
      describe "tv :: t -> [Ty.Tyvar]" $
        it "returns variables, in left-to-right order, no duplicates" $ do
          let --d  = (Ty.Tyvar "d"  Knd.Star)
              dot = (TVar b `fn` TVar c) `fn` (TVar a `fn` TVar b) `fn` (TVar a `fn` TVar c)
              dlr = (TVar a `fn` TVar b) `fn` TVar a `fn` TVar b
              --f  = (Ty.Tyvar "f"  (Knd.Kfun Knd.Star Knd.Star))
              --m  = (Ty.Tyvar "m"  (Knd.Kfun Knd.Star Knd.Star))
              --mt = (Ty.Tyvar "mt" (Knd.Kfun (Knd.Kfun (Knd.Kfun Knd.Star Knd.Star) Knd.Star) Knd.Star))
          tv Ty.tInt              `shouldBe` []
          tv (TVar a)             `shouldBe` [a]
          tv (TVar a `fn` TVar b) `shouldBe` [a, b]
          tv dot                  `shouldBe` [b, c, a]
          tv dlr                  `shouldBe` [a, b]
    describe "instance Types a => Types [a]" $ do
      describe "apply :: Subst -> t -> t" $
        it "is" pending
      describe "tv :: t -> [Ty.Tyvar]" $
        it "returns variables, in left-to-right order, no duplicates" $ do
          tv [TVar a, TVar b, TVar c] `shouldBe` [a, b, c]
          tv (subsequences [TVar a, TVar b, TVar c, TVar b]) `shouldBe` [a, b, c]
