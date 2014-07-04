module TypecheckerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "reverse" $ do
    it "reverses a list 1" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]
    it "reverses a list 2" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" $ property $
      \xs -> (reverse . reverse) xs == (xs :: [Int])
