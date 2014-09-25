module Test.Luna.Typechecker.AST.LitSpec (spec) where


import Luna.Typechecker.TIMonad         (startTI)
import Luna.Typechecker.Typeclasses     (Pred(..))

import Luna.Typechecker.AST.Lit
import Luna.Typechecker.AST.Type        (tChar, tInteger, tFloat, tString)

import Luna.Typechecker.Internal.Logger

import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
  describe "tiLit" $ do
    it "just works for Char" $ property $
      \x -> startTI (evalLoggerT (tiLit (LitChar x)))  `shouldBe` Right ([], tChar)
    it "just works for Float" $ property $
      \x -> startTI (evalLoggerT (tiLit (LitFloat x))) `shouldBe` Right ([], tFloat)
    it "just works for Int" $ property $
      \x -> startTI (evalLoggerT (tiLit (LitInt x)))   `shouldBe` Right ([], tInteger)
    it "just works for Integral" $ property $
      \x -> let Right (ps, t) = startTI $ evalLoggerT (tiLit (LitIntegral x))
             in ps `shouldContain` [IsIn "Integral" t]
    it "just works for String" $ property $
      \x -> startTI (evalLoggerT (tiLit (LitStr x))) `shouldBe` Right ([], tString)
  describe "(coverage booster)" $
    describe "instance Show Lit" $
      it "show :: a -> String" $
        length (concatMap show [LitChar 'c', LitFloat 1.0, LitInt (1 :: Integer), LitIntegral 1, LitStr "lel"]) `shouldSatisfy` (>0)
