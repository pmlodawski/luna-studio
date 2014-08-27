module Test.Luna.Typechecker.Internal.AST.TypeSpec (spec) where

import qualified Luna.Typechecker.Internal.AST.Alternatives as Alt
import qualified Luna.Typechecker.Internal.AST.Common       as Cmm
import qualified Luna.Typechecker.Internal.AST.Expr         as Exp
import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Luna.Typechecker.Internal.AST.Lit          as Lit
import qualified Luna.Typechecker.Internal.AST.Module       as Mod
import qualified Luna.Typechecker.Internal.AST.Pat          as Pat
import qualified Luna.Typechecker.Internal.AST.Scheme       as Sch
import qualified Luna.Typechecker.Internal.AST.TID          as TID

import qualified Luna.Typechecker.Internal.Ambiguity        as Amb
import qualified Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Luna.Typechecker.Internal.BindingGroups    as Bnd
import qualified Luna.Typechecker.Internal.ContextReduction as CxR
import qualified Luna.Typechecker.Internal.HasKind          as HKd
import qualified Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl
import qualified Luna.Typechecker.Internal.TypeInference    as Inf
import qualified Luna.Typechecker.Internal.Unification      as Uni
import qualified Luna.Typechecker                           as Typechecker

import           Luna.Typechecker.Internal.AST.Type

import           Test.Hspec

spec :: Spec
spec = do
  describe "predefined types" $
    it "shall yield kind * when appropriate" $ do
      HKd.kind tUnit                                `shouldBe` Knd.Star
      HKd.kind tInt                                 `shouldBe` Knd.Star
      HKd.kind (list tInt)                          `shouldBe` Knd.Star
      HKd.kind (tInt `fn` tInt)                     `shouldBe` Knd.Star
      HKd.kind (list tInt `fn` tInt)                `shouldBe` Knd.Star
      HKd.kind (tInt `pair` tInt)                   `shouldBe` Knd.Star
      HKd.kind (tInt `pair` tUnit)                  `shouldBe` Knd.Star
      HKd.kind ((tInt `fn` tInt) `pair` list tUnit) `shouldBe` Knd.Star
  describe "fn" $ do
    it "should verify kinds" $ pending -- needs `fn` to be partial
  describe "list" $ do
    it "should verify kinds" $ pending -- needs `list` to be partial
  describe "pair" $ do
    it "should verify kinds" $ pending -- needs `pair` to be partial
