module Test.Luna.Typechecker.Internal.AssumptionsSpec (spec) where

--import Luna.Typechecker.Internal.AST.Alternatives as Alt
--import Luna.Typechecker.Internal.AST.Common       as Cmm
--import Luna.Typechecker.Internal.AST.Expr         as Exp
import Luna.Typechecker.Internal.AST.Kind
--import Luna.Typechecker.Internal.AST.Lit          as Lit
--import Luna.Typechecker.Internal.AST.Module       as Mod
--import Luna.Typechecker.Internal.AST.Pat          as Pat
import Luna.Typechecker.Internal.AST.Scheme
--import Luna.Typechecker.Internal.AST.TID          as TID
import Luna.Typechecker.Internal.AST.Type


--import Luna.Typechecker.Internal.Ambiguity        as Amb
import Luna.Typechecker.Internal.Assumptions
import Luna.Typechecker.Internal.BindingGroups
--import Luna.Typechecker.Internal.ContextReduction as CxR
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