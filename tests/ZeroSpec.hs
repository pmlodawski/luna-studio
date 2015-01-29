module ZeroSpec (spec) where

import Test.Hspec



spec :: Spec
spec =
    describe "testing framework" $
        it "basic math works" $
            1 + 2 `shouldBe` (3 :: Int)