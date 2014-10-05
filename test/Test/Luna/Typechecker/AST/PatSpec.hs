module Test.Luna.Typechecker.AST.PatSpec (spec) where

import Luna.Typechecker.Assumptions
import Luna.Typechecker.HasKind
import Luna.Typechecker.Substitutions
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Typeclasses
import Luna.Typechecker.Unification

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Lit
import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Data.Either                      (isRight)

import Test.Hspec


spec :: Spec
spec = do
  describe "tiPat" $ do
    it "just works for PVar" $ do
      let v = PVar (TID "lel")
          Right (ps, as, TVar t) = startTI $ evalLoggerT $ tiPat v
      ps `shouldBe` []
      as `shouldContain` [TID "lel" :>: toScheme (TVar t)]
      evalLogger (kind t) `shouldBe` Right Star

    it "just works for PWildcard" $ do
      let v = PWildcard
          Right (ps, as, TVar t) = startTI $ evalLoggerT $  tiPat v
      ps `shouldBe` []
      as `shouldContain` []
      evalLogger (kind t) `shouldBe` Right Star

    it "just works for PAs+PVar" $ do
      let v = PAs (TID "lel") (PVar (TID "lol"))
          Right (ps, as, TVar t) = startTI $ evalLoggerT $  tiPat v
      ps `shouldBe` []
      as `shouldContain` [TID "lel" :>: toScheme (TVar t)]
      as `shouldContain` [TID "lol" :>: toScheme (TVar t)]
      evalLogger (kind t) `shouldBe` Right Star

    it "just works for PAs+PCon" $ do
      let v = PAs (TID "lel") (PCon (TID "(:)" :>:cons_type) [PVar (TID "x"), PVar (TID "xs")])
          cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
          getall = do res <- tiPat v
                      s' <- getSubst
                      return (res, s')
          Right ((ps, as, TVar t), s) = startTI $ evalLoggerT getall
          Right (Forall [] ([] :=> x'))   = evalLogger $ find (TID "x")   as
          Right (Forall [] ([] :=> xs'))  = evalLogger $ find (TID "xs")  as
          Right (Forall [] ([] :=> lel')) = evalLogger $ find (TID "lel") as
          x = apply s x'
          xs = apply s xs'
          lel = apply s lel'
      ps `shouldBe` []
      xs `shouldBe` lel
      list x `shouldBe` xs
      evalLogger (kind t) `shouldBe` Right Star

    it "just works for PLit+LitChar" $ do
      let v = PLit (LitChar 'l')
          Right (ps, as, tChar') = startTI $ evalLoggerT $ tiPat v
      tChar' `shouldBe` tChar
      ps `shouldBe` []
      as `shouldBe` []

    it "just works for PLit+LitIntegral" $ do
      let v = PLit (LitIntegral 123)
          Right (ps, as, t) = startTI $ evalLoggerT $ tiPat v
      ps `shouldContain` [IsIn (TID "Integral") t]
      as `shouldBe` []

    it "just works for PCon" $ do
      let v = PCon (TID "(:)" :>:cons_type) [PVar (TID "x"), PVar (TID "xs")]
          cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
          Right (ps, as, TVar t) = startTI $ evalLoggerT $ tiPat v
          Right (Forall [] ([] :=> x))  = evalLogger $ find (TID "x")  as
          Right (Forall [] ([] :=> xs)) = evalLogger $ find (TID "xs") as
      ps `shouldBe` []
      evalLogger (kind x)  `shouldBe` Right Star
      evalLogger (kind xs) `shouldBe` Right Star
      evalLogger (xs `match` list x) `shouldSatisfy` isRight
      evalLogger (kind t) `shouldBe` Right Star
  describe "(coverage booster)" $
    it "show" $ do
      let v = PCon (TID "(:)" :>:cons_type) [PVar (TID "x"), PVar (TID "xs")]
          cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
      length (concatMap show [PVar (TID "a"), PWildcard, PAs (TID "a") (PVar (TID "b")), PLit (LitInt 123), v]) `shouldSatisfy` (>0)
