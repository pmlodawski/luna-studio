module Test.Luna.Typechecker.Internal.ContextReductionSpec (spec) where

--import Luna.Typechecker.Internal.AST.Alternatives as Alt
--import Luna.Typechecker.Internal.AST.Common       as Cmm
--import Luna.Typechecker.Internal.AST.Expr         as Exp
import Luna.Typechecker.Internal.AST.Kind
--import Luna.Typechecker.Internal.AST.Lit          as Lit
--import Luna.Typechecker.Internal.AST.Module       as Mod
--import Luna.Typechecker.Internal.AST.Pat          as Pat
--import Luna.Typechecker.Internal.AST.Scheme       as Sch
--import Luna.Typechecker.Internal.AST.TID          as TID
import Luna.Typechecker.Internal.AST.Type


--import Luna.Typechecker.Internal.Ambiguity        as Amb
--import Luna.Typechecker.Internal.Assumptions      as Ass
--import Luna.Typechecker.Internal.BindingGroups    as Bnd
import Luna.Typechecker.Internal.ContextReduction
--import Luna.Typechecker.Internal.HasKind          as HKd
--import Luna.Typechecker.Internal.Substitutions    as Sub
--import Luna.Typechecker.Internal.TIMonad          as TIM
import Luna.Typechecker.Internal.Typeclasses
--import Luna.Typechecker.Internal.TypeInference    as Inf
--import Luna.Typechecker.Internal.Unification      as Uni
--import Luna.Typechecker                           as Typechecker

import Test.Hspec
import Control.Exception

spec :: Spec
spec = do
  describe "inHnf" $ do
    it "verifies the result" $ do
      inHnf (IsIn "anything" (TVar $ Tyvar "a" Star)) `shouldBe` True
      inHnf (IsIn "anything" (TCon $ Tycon "Int" Star)) `shouldBe` False
      inHnf (IsIn "anything" (list (TCon $ Tycon "Int" Star))) `shouldBe` False
      inHnf (IsIn "anything" (list (TVar $ Tyvar "a" Star))) `shouldBe` False
      inHnf (IsIn "anything" (TAp (TVar $ Tyvar "m" Star) (TCon $ Tycon "Int" Star))) `shouldBe` True
      evaluate (inHnf (IsIn "anything" (TGen 0))) `shouldThrow` anyErrorCall

