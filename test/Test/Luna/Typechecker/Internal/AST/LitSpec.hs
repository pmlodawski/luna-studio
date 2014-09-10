module Test.Luna.Typechecker.Internal.AST.LitSpec (spec) where

import Luna.Typechecker.Internal.AST.Lit
import Luna.Typechecker.Internal.TIMonad     (runTI)
import Luna.Typechecker.Internal.Typeclasses (Pred(..))
import Luna.Typechecker.Internal.AST.Type    (tChar, tInteger, tFloat, tString)

import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
  describe "tiLit" $ do
    it "just works for Char" $ property $
      \x -> runTI (tiLit (LitChar x)) `shouldBe` ([], tChar)
    it "just works for Float" $ property $
      \x -> runTI (tiLit (LitFloat x)) `shouldBe` ([], tFloat)
    it "just works for Int" $ property $
      \x -> runTI (tiLit (LitInt x)) `shouldBe` ([], tInteger)
    it "just works for Integral" $ property $
      \x -> let (ps, t) = runTI (tiLit (LitIntegral x))
             in ps `shouldContain` [IsIn "Integral" t]
    it "just works for String" $ property $
      \x -> runTI (tiLit (LitStr x)) `shouldBe` ([], tString)
  describe "(coverage booster)" $ do
    describe "instance Show Lit" $ do
      it "show :: a -> String" $ do
        length (concatMap show [LitChar 'c', LitFloat 1.0, LitInt (1 :: Integer), LitIntegral 1, LitStr "lel"]) `shouldSatisfy` (>0)