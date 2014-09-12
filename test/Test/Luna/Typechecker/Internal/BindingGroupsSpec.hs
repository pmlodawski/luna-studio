module Test.Luna.Typechecker.Internal.BindingGroupsSpec (spec) where

--import Luna.Typechecker.Internal.AST.Alternatives
--import Luna.Typechecker.Internal.AST.Common
--import Luna.Typechecker.Internal.AST.Expr
import Luna.Typechecker.Internal.AST.Kind
--import Luna.Typechecker.Internal.AST.Lit
--import Luna.Typechecker.Internal.AST.Module
import Luna.Typechecker.Internal.AST.Pat
import Luna.Typechecker.Internal.AST.Scheme
import Luna.Typechecker.Internal.AST.TID
import Luna.Typechecker.Internal.AST.Type


--import Luna.Typechecker.Internal.Ambiguity
import Luna.Typechecker.Internal.Assumptions
import Luna.Typechecker.Internal.BindingGroups
--import Luna.Typechecker.Internal.ContextReduction
--import Luna.Typechecker.Internal.HasKind
--import Luna.Typechecker.Internal.Substitutions
import Luna.Typechecker.Internal.TIMonad
import Luna.Typechecker.Internal.Typeclasses
--import Luna.Typechecker.Internal.TypeInference
--import Luna.Typechecker.Internal.Unification
--import Luna.Typechecker

import Test.Hspec
import Control.Exception


spec :: Spec
spec = do
  describe "tiExpl" $ do
    it "fails for too weak contexts" $ do
      let inf       = tiExpl ce ["(==)":>:eq_type] ("lel", lel_type, [(lel_pat1, lel_body1)])
          eq_type   = Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
          lel_type  = Forall [Star] ([] :=> (TGen 0 `fn` tBool))
          lel_pat1  = [PVar "x"]
          lel_body1 = foldl1 Ap [Var "(==)", Var "x", Var "x"]
          Just ce   = (  addClass "Eq" []
                     <:> addInst [] (IsIn "Eq" tInt)
                      ) initialEnv
      evaluate (runTI inf) `shouldThrow` anyErrorCall
    it "fails for too general signatures" $ do
      let inf       = tiExpl ce ["(==)":>:eq_type] ("lel", lel_type, [(lel_pat1, lel_body1)])
          eq_type   = Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
          lel_type  = Forall [Star, Star] ([] :=> (TGen 0 `fn` TGen 1))
          lel_pat1  = [PVar "x"]
          lel_body1 = foldl1 Ap [Var "(==)", Var "x", Var "x"]
          Just ce   = (  addClass "Eq" []
                     <:> addInst [] (IsIn "Eq" tInt)
                      ) initialEnv
      evaluate (runTI inf) `shouldThrow` anyErrorCall

  --describe "tiBindGroup" $ it "is" pending
  --describe "tiSeq" $ it "is" pending
  --describe "(internal)" $ do
  --  describe "tiExpr" $ do

    --describe "tiAlt" $ it "is" pending
    --describe "tiAlts" $ it "is" pending
    --describe "tiExpl" $ it "is" pending
    --describe "restricted" $ it "is" pending
    --describe "tiImpls" $ it "is" pending

