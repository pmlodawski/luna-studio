module Test.Luna.Typechecker.HasKindSpec (spec) where

--import qualified Luna.Typechecker.AST.Alternatives as Alt
--import qualified Luna.Typechecker.AST.Common       as Cmm
--import qualified Luna.Typechecker.AST.Expr         as Exp
--import qualified Luna.Typechecker.AST.Lit          as Lit
--import qualified Luna.Typechecker.AST.Module       as Mod
--import qualified Luna.Typechecker.AST.Pat          as Pat
--import qualified Luna.Typechecker.AST.Scheme       as Sch
--import qualified Luna.Typechecker.AST.TID          as TID
import qualified Luna.Typechecker.AST.Type         as Ty

import           Luna.Typechecker.AST.Kind         (Kind(..))

--import qualified Luna.Typechecker.Ambiguity        as Amb
--import qualified Luna.Typechecker.Assumptions      as Ass
--import qualified Luna.Typechecker.BindingGroups    as Bnd
--import qualified Luna.Typechecker.ContextReduction as CxR
--import qualified Luna.Typechecker.Substitutions    as Sub
--import qualified Luna.Typechecker.TIMonad          as TIM
--import qualified Luna.Typechecker.Typeclasses      as Tcl
--import qualified Luna.Typechecker.TypeInference    as Inf
--import qualified Luna.Typechecker.Unification      as Uni
--import qualified Luna.Typechecker                           as Typechecker

import           Luna.Typechecker.HasKind          (kind)


import           Test.Hspec
--import           Test.QuickCheck
import           Control.Exception                          (evaluate)


spec :: Spec
spec =
  describe "class HasKind t" $ do
    describe "instance HasKind Tyvar" $
      it "kind :: t -> Kind" $ do
        kind (Ty.Tyvar undefined Star)             `shouldBe` Star
        kind (Ty.Tyvar undefined (Kfun Star Star)) `shouldBe` (Kfun Star Star)
    describe "instance HasKind Tycon" $
      it "kind :: t -> Kind" $ do
        kind (Ty.Tycon undefined Star)             `shouldBe` Star
        kind (Ty.Tycon undefined (Kfun Star Star)) `shouldBe` (Kfun Star Star)
    describe "instance HasKind Type" $
      it "kind :: t -> Kind" $ do
        kind Ty.tUnit                              `shouldBe` Star
        kind Ty.tChar                              `shouldBe` Star
        kind Ty.tInt                               `shouldBe` Star
        kind Ty.tInteger                           `shouldBe` Star
        kind Ty.tFloat                             `shouldBe` Star
        kind Ty.tDouble                            `shouldBe` Star

        kind Ty.tList                              `shouldBe` Kfun Star Star
        kind Ty.tArrow                             `shouldBe` Kfun Star (Kfun Star Star)
        kind Ty.tTuple2                            `shouldBe` Kfun Star (Kfun Star Star)

        kind Ty.tString                            `shouldBe` Star

        kind (Ty.TVar $ Ty.Tyvar undefined Star)   `shouldBe` Star
        kind (Ty.TCon $ Ty.Tycon undefined Star)   `shouldBe` Star
        kind (Ty.TAp Ty.tList Ty.tInt)             `shouldBe` Star
        evaluate (kind (Ty.TAp Ty.tList Ty.tList)) `shouldThrow` errorCall "kind mismatch"
        evaluate (kind (Ty.TGen undefined))        `shouldThrow` anyErrorCall
