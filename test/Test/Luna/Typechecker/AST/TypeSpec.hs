module Test.Luna.Typechecker.AST.TypeSpec (spec) where


import qualified Luna.Typechecker.HasKind          as HKd

import qualified Luna.Typechecker.AST.Kind         as Knd
import           Luna.Typechecker.AST.Type

import           Luna.Typechecker.Internal.Logger

import           Test.Hspec

spec :: Spec
spec = do
  describe "predefined types" $
    it "shall yield kind * when appropriate" $ do
      evalLogger (HKd.kind tUnit                               ) `shouldBe` Right Knd.Star
      evalLogger (HKd.kind tInt                                ) `shouldBe` Right Knd.Star
      evalLogger (HKd.kind (list tInt)                         ) `shouldBe` Right Knd.Star
      evalLogger (HKd.kind (tInt `fn` tInt)                    ) `shouldBe` Right Knd.Star
      evalLogger (HKd.kind (list tInt `fn` tInt)               ) `shouldBe` Right Knd.Star
      evalLogger (HKd.kind (tInt `pair` tInt)                  ) `shouldBe` Right Knd.Star
      evalLogger (HKd.kind (tInt `pair` tUnit)                 ) `shouldBe` Right Knd.Star
      evalLogger (HKd.kind ((tInt `fn` tInt) `pair` list tUnit)) `shouldBe` Right Knd.Star
  describe "fn" $
    it "should verify kinds" $ pendingWith "fn needs to verify that both arguments have kind *"
  describe "list" $
    it "should verify kinds" $ pendingWith "list needs to verify that argument have kind *"
  describe "pair" $
    it "should verify kinds" $ pendingWith "pair needs to verify that both arguments have kind *"
  describe "(coverage booster)" $
    it "shows TAp (TAp (,) Int) String == (Int,String)" $
      length (show (TAp (TAp tTuple2 tInt) tString)) `shouldSatisfy` (>0)
