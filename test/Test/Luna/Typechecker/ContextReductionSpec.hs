module Test.Luna.Typechecker.ContextReductionSpec (spec) where

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Type


import Luna.Typechecker.ContextReduction
import Luna.Typechecker.Typeclasses



import Test.Hspec

import Control.Exception


spec :: Spec
spec = do
  describe "inHnf" $
    it "verifies the result" $ do
      inHnf (IsIn "anything" (TVar $ Tyvar "a" Star)) `shouldBe` True
      inHnf (IsIn "anything" (TCon $ Tycon "Int" Star)) `shouldBe` False
      inHnf (IsIn "anything" (list (TCon $ Tycon "Int" Star))) `shouldBe` False
      inHnf (IsIn "anything" (list (TVar $ Tyvar "a" Star))) `shouldBe` False
      inHnf (IsIn "anything" (TAp (TVar $ Tyvar "m" Star) (TCon $ Tycon "Int" Star))) `shouldBe` True
      evaluate (inHnf (IsIn "anything" (TGen 0))) `shouldThrow` anyErrorCall
  describe "toHnf" $ do
    it "works for bad input" $ do
      let Just ce = (     addClass "Eq" []
                      <:> addInst [] (IsIn "Eq" tBool))
                    initialEnv
          res = toHnf initialEnv (IsIn "anything" (TCon $ Tycon "Int" Star)) :: Either String [Pred]
          res2 = toHnf ce (IsIn "Eq" tBool) :: Either String [Pred]
          res3 = toHnf ce (IsIn "Eq" tInt) :: Either String [Pred]
      evaluate res `shouldThrow` anyErrorCall
      res2 `shouldBe` Right []
      evaluate res3 `shouldThrow` anyErrorCall

    it "covers nested predicates" $
      let Just ce = (  addClass "Eq" []
                   <:> addClass "Ord" ["Eq"]
                   <:> addClass "Num" []
                   <:> addClass "Real" ["Num", "Ord"]
                   <:> addClass "Enum" []
                   <:> addClass "Integral" ["Real", "Enum"]
                   <:> addClass "Fractional" ["Num"]
                   <:> addClass "Functor" []
                   <:> addInst [] (IsIn "Eq" tInt)             <:> addInst [] (IsIn "Eq" tInteger)       <:> addInst [] (IsIn "Eq" tDouble)   <:> addInst [] (IsIn "Eq" tFloat)
                   <:> addInst [] (IsIn "Ord" tInt)            <:> addInst [] (IsIn "Ord" tInteger)      <:> addInst [] (IsIn "Ord" tDouble)  <:> addInst [] (IsIn "Ord" tFloat)
                   <:> addInst [] (IsIn "Num" tInt)            <:> addInst [] (IsIn "Num" tInteger)      <:> addInst [] (IsIn "Num" tDouble)  <:> addInst [] (IsIn "Num" tFloat)
                   <:> addInst [] (IsIn "Real" tInt)           <:> addInst [] (IsIn "Real" tInteger)     <:> addInst [] (IsIn "Real" tDouble) <:> addInst [] (IsIn "Real" tFloat)
                   <:> addInst [] (IsIn "Enum" tInt)           <:> addInst [] (IsIn "Enum" tInteger)
                   <:> addInst [] (IsIn "Integral" tInt)       <:> addInst [] (IsIn "Integral" tInteger)
                   <:> addInst [] (IsIn "Fractional" tDouble)  <:> addInst [] (IsIn "Fractional" tFloat)
                   <:> addInst [] (IsIn "Functor" tList)       <:> addInst [] (IsIn "Functor" tMaybe)
                   <:> addInst [IsIn "Functor" tv_f2, IsIn "Num" tv_a1] (IsIn "Num" (TAp tv_f2 tv_a1)) -- nonsense, I know
                   <:> addInst [IsIn "Functor" tv_f2, IsIn "Ord" tv_a1]
                               (IsIn "Ord" (TAp tv_f2 tv_a1))
                    ) initialEnv

          tMaybe = TCon $ Tycon "Maybe" $ Star `Kfun` Star

          tv_a1 = TVar $ Tyvar "a" Star
          tv_f2 = TVar $ Tyvar "f" (Star `Kfun` Star)

          p = IsIn "Num" (TAp tMaybe tv_a1)

          Just res = toHnf ce p
       in res `shouldSatisfy` any (\(IsIn name _) -> name == "Num")


