{-# LANGUAGE ScopedTypeVariables #-}

module Test.Luna.Typechecker.TIMonadSpec (spec) where

import Luna.Typechecker.TIMonad
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.Type

--import Luna.Typechecker.Internal.Logger -- evalLogger
--import Data.Either                      (isLeft)

import Luna.Typechecker.Internal.Logger

import Test.Hspec
import Test.QuickCheck

import Test.Luna.Typechecker.AST.TypeGen (genTypeNogen,genPredNogen)


spec :: Spec
spec = do
  describe "freshInst" $ do
    
    it "works for simple types [QC]" $ property $ 
      forAll (genTypeNogen Star) $ \t -> do
        let qt = [] :=> t
            Right x  = runTI $ evalLoggerT $ freshInst (Forall [] qt)
        x `shouldBe` qt

    it "works with one predicate" $ do
      let a  = TVar $ Tyvar "a" Star
          ps = [ IsIn "Integral" a ]
          qt = ps :=> (a `fn` a)
          Right x  = runTI $ evalLoggerT $ freshInst (Forall [] qt)
      x `shouldBe` qt

    it "works with one predicate and one gen-var" $ do
      let a  = TGen 0
          ps = [ IsIn "Integral" a ]
          qt = ps :=> (a `fn` a)
          Right ([IsIn "Integral" t'] :=> t'') = runTI $ evalLoggerT $ freshInst (Forall [Star] qt)
      t'' `shouldBe` (t' `fn` t')

  describe "(internals)" $
    describe "class Instantiate t" $
      describe "instance Instantiate a => Instantiate [a]" $ do
        it "inst [] is an identity for x::Type  (no TGen inside!) [qc]" $ property $
          forAll arbitrary        $ \k ->
          forAll (genTypeNogen k) $ \x ->
            inst [] (x::Type) `shouldBe` x
        it "inst [] is an identity for x::(Qual Type)  (no TGen inside!) [qc]" $ property $
          forAll arbitrary         $ \k1  ->
          forAll arbitrary         $ \k2  ->
          forAll (genPredNogen k1) $ \ps ->
          forAll (genTypeNogen k2) $ \t  ->
            inst [] ([ps] :=> t) `shouldBe` ([ps] :=> t)
        it "inst [] is an identity for x::Pred  (no TGen inside!) [qc]" $ property $
          forAll arbitrary        $ \c  ->
          forAll arbitrary        $ \k  ->
          forAll (genTypeNogen k) $ \t  ->
            inst [] (IsIn c t) `shouldBe` IsIn c t
