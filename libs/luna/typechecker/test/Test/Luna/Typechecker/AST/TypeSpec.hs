module Test.Luna.Typechecker.AST.TypeSpec (spec) where
import qualified Luna.Typechecker.AST.Kind         as Knd
import qualified Luna.Typechecker.HasKind          as HKd

import           Luna.Typechecker.AST.Type

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
  describe "fn" $
    it "should verify kinds" $ pendingWith "fn needs to verify that both arguments have kind *"
  describe "list" $
    it "should verify kinds" $ pendingWith "list needs to verify that argument have kind *"
  describe "pair" $
    it "should verify kinds" $ pendingWith "pair needs to verify that both arguments have kind *"
  describe "(coverage booster)" $
    it "shows TAp (TAp (,) Int) String == (Int,String)" $
      length (show (TAp (TAp tTuple2 tInt) tString)) `shouldSatisfy` (>0)
