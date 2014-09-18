module Test.Luna.Typechecker.AssumptionsSpec (spec) where

--import Luna.Typechecker.AST.Alternatives as Alt
--import Luna.Typechecker.AST.Common       as Cmm
--import Luna.Typechecker.AST.Expr         as Exp
import Luna.Typechecker.AST.Kind
--import Luna.Typechecker.AST.Lit          as Lit
--import Luna.Typechecker.AST.Module       as Mod
--import Luna.Typechecker.AST.Pat          as Pat
import Luna.Typechecker.AST.Scheme
--import Luna.Typechecker.AST.TID          as TID
import Luna.Typechecker.AST.Type


--import Luna.Typechecker.Ambiguity        as Amb
import Luna.Typechecker.Assumptions
--import Luna.Typechecker.BindingGroups
--import Luna.Typechecker.ContextReduction as CxR
--import Luna.Typechecker.HasKind          as HKd
--import Luna.Typechecker.Substitutions    as Sub
--import Luna.Typechecker.TIMonad          as TIM
import Luna.Typechecker.Typeclasses
--import Luna.Typechecker.TypeInference    as Inf
--import Luna.Typechecker.Unification      as Uni
--import Luna.Typechecker                           as Typechecker

import Test.Hspec
import Control.Exception

spec :: Spec
spec =
  describe "find" $ do
    it "can fail" $ do
      let res = find "a" [] :: Either String Scheme
      evaluate res `shouldThrow` anyErrorCall
    it "works for singletons" $ do
      let res = find "a" ["a" :>: sch]
          sch = (Forall [] ([] :=> (TVar $ Tyvar "a" Star)))
      evaluate res `shouldReturn` Just sch
    it "recurses" $ do
      let res = find "a" ["b" :>: sch2, "a" :>: sch1]
          sch1 = (Forall [] ([] :=> (TVar $ Tyvar "a" Star)))
          sch2 = (Forall [] ([] :=> (TVar $ Tyvar "b" Star)))
      evaluate res `shouldReturn` Just sch1
