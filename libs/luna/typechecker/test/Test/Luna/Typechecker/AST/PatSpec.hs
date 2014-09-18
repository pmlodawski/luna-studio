module Test.Luna.Typechecker.AST.PatSpec (spec) where

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Lit
import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.Type
import Luna.Typechecker.Assumptions
import Luna.Typechecker.HasKind
import Luna.Typechecker.Substitutions
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Typeclasses
import Luna.Typechecker.Unification

import Data.Maybe
import Test.Hspec

spec :: Spec
spec = do
  describe "tiPat" $ do
    it "just works for PVar" $ do
      let v = PVar "lel"
          (ps, as, TVar t) = runTI $ tiPat v
      ps `shouldBe` []
      as `shouldContain` ["lel" :>: toScheme (TVar t)]
      kind t `shouldBe` Star

    it "just works for PWildcard" $ do
      let v = PWildcard
          (ps, as, TVar t) = runTI $ tiPat v
      ps `shouldBe` []
      as `shouldContain` []
      kind t `shouldBe` Star

    it "just works for PAs+PVar" $ do
      let v = PAs "lel" (PVar "lol")
          (ps, as, TVar t) = runTI $ tiPat v
      ps `shouldBe` []
      as `shouldContain` ["lel" :>: toScheme (TVar t)]
      as `shouldContain` ["lol" :>: toScheme (TVar t)]
      kind t `shouldBe` Star

    it "just works for PAs+PCon" $ do
      let v = PAs "lel" (PCon ("(:)":>:cons_type) [PVar "x", PVar "xs"])
          cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
          getall = do res <- tiPat v
                      s' <- getSubst
                      return (res, s')
          ((ps, as, TVar t), s) = runTI getall
          Just (Forall [] ([] :=> x'))  = find "x"  as
          Just (Forall [] ([] :=> xs')) = find "xs" as
          Just (Forall [] ([] :=> lel')) = find "lel" as
          x = apply s x'
          xs = apply s xs'
          lel = apply s lel'
      ps `shouldBe` []
      xs `shouldBe` lel
      list x `shouldBe` xs
      kind t `shouldBe` Star

    it "just works for PLit+LitChar" $ do
      let v = PLit (LitChar 'l')
          (ps, as, tChar') = runTI $ tiPat v
      tChar' `shouldBe` tChar
      ps `shouldBe` []
      as `shouldBe` []

    it "just works for PLit+LitIntegral" $ do
      let v = PLit (LitIntegral 123)
          (ps, as, t) = runTI $ tiPat v
      ps `shouldContain` [IsIn "Integral" t]
      as `shouldBe` []

    it "just works for PCon" $ do
      let v = PCon ("(:)":>:cons_type) [PVar "x", PVar "xs"]
          cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
          (ps, as, TVar t) = runTI $ tiPat v
          Just (Forall [] ([] :=> x))  = find "x"  as
          Just (Forall [] ([] :=> xs)) = find "xs" as
      ps `shouldBe` []
      kind x  `shouldBe` Star
      kind xs `shouldBe` Star
      (xs `match` list x) `shouldSatisfy` isJust
      kind t `shouldBe` Star
  describe "(coverage booster)" $
    it "show" $ do
      let v = PCon ("(:)":>:cons_type) [PVar "x", PVar "xs"]
          cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
      length (concatMap show [PVar "a", PWildcard, PAs "a" (PVar "b"), PLit (LitInt 123), v]) `shouldSatisfy` (>0)
