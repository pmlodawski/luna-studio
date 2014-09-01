{-# LANGUAGE ScopedTypeVariables #-}

module Test.Luna.Typechecker.Internal.TIMonadSpec (spec) where

--import Luna.Typechecker.Internal.AST.Alternatives as Alt
--import Luna.Typechecker.Internal.AST.Common       as Cmm
--import Luna.Typechecker.Internal.AST.Expr         as Exp
--import Luna.Typechecker.Internal.AST.Kind         as Knd
--import Luna.Typechecker.Internal.AST.Lit          as Lit
--import Luna.Typechecker.Internal.AST.Module       as Mod
--import Luna.Typechecker.Internal.AST.Pat          as Pat
--import Luna.Typechecker.Internal.AST.Scheme       as Sch
--import Luna.Typechecker.Internal.AST.TID          as TID
import Luna.Typechecker.Internal.AST.Type         (Type(..))


--import Luna.Typechecker.Internal.Ambiguity        as Amb
--import Luna.Typechecker.Internal.Assumptions      as Ass
--import Luna.Typechecker.Internal.BindingGroups    as Bnd
--import Luna.Typechecker.Internal.ContextReduction as CxR
--import Luna.Typechecker.Internal.HasKind          as HKd
--import Luna.Typechecker.Internal.Substitutions    as Sub
import Luna.Typechecker.Internal.TIMonad          (Instantiate(..))
import Luna.Typechecker.Internal.Typeclasses      (Pred(..), Qual(..))
--import Luna.Typechecker.Internal.TypeInference    as Inf
--import Luna.Typechecker.Internal.Unification      as Uni
--import Luna.Typechecker                           as Typechecker

import Test.Luna.Typechecker.Internal.AST.TypeGen (genTypeNogen,genPredNogen)

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "freshInst" $ do
    it "is an identity when ks==[], x::Type (no TGen inside!)" $ property $
      forAll arbitrary        $ \k ->
      forAll (genTypeNogen k) $ \x ->
        inst [] (x::Type) `shouldBe` x
    it "is an identity when ks==[], x::Qual Type (no TGen inside!)" $ property $
      forAll arbitrary         $ \k1  ->
      forAll arbitrary         $ \k2  ->
      forAll (genPredNogen k1) $ \ps ->
      forAll (genTypeNogen k2) $ \t  ->
        inst [] ([ps] :=> t) `shouldBe` ([ps] :=> t)
    it "is an identity when ks==[], x::Pred (no TGen inside!)" $ property $
      forAll arbitrary        $ \c  ->
      forAll arbitrary        $ \k  ->
      forAll (genTypeNogen k) $ \t  ->
        inst [] (IsIn c t) `shouldBe` (IsIn c t)

  describe "Luna/Typechecker/Internal/TIMonad.hs" $ it "is" pending
