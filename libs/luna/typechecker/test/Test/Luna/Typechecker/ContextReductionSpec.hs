module Test.Luna.Typechecker.ContextReductionSpec (spec) where


import Luna.Typechecker.ContextReduction
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Control.Exception

import Data.Either                       (isLeft)
import Data.Functor.Identity             (runIdentity)

import Test.Hspec


spec :: Spec
spec = do
  describe "inHnf" $
    it "verifies the result" $ do
      (evalLogger $ inHnf (IsIn "anything" (TVar $ Tyvar "a" Star))) `shouldBe` Right True
      (evalLogger $ inHnf (IsIn "anything" (TCon $ Tycon "Int" Star))) `shouldBe` Right False
      (evalLogger $ inHnf (IsIn "anything" (list (TCon $ Tycon "Int" Star)))) `shouldBe` Right False
      (evalLogger $ inHnf (IsIn "anything" (list (TVar $ Tyvar "a" Star)))) `shouldBe` Right False
      (evalLogger $ inHnf (IsIn "anything" (TAp (TVar $ Tyvar "m" Star) (TCon $ Tycon "Int" Star)))) `shouldBe` Right True
      (evalLogger $ (inHnf (IsIn "anything" (TGen 0)))) `shouldSatisfy` isLeft
  describe "toHnf" $ do
    it "works for bad input" $ do
      let Right ce = evalLogger ((     addClass "Eq" []
                      <:> addInst [] (IsIn "Eq" tBool))
                    initialEnv)
          res = evalLogger (toHnf initialEnv (IsIn "anything" (TCon $ Tycon "Int" Star)))
          res2 = evalLogger (toHnf ce (IsIn "Eq" tBool))
          res3 = evalLogger (toHnf ce (IsIn "Eq" tInt))
      res `shouldSatisfy` isLeft
      res2 `shouldBe` Right []
      res3 `shouldSatisfy` isLeft

    it "covers nested predicates" $
      let Right ce = evalLogger ((  addClass "Eq" []
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
                          ) initialEnv)

          tMaybe = TCon $ Tycon "Maybe" $ Star `Kfun` Star

          tv_a1 = TVar $ Tyvar "a" Star
          tv_f2 = TVar $ Tyvar "f" (Star `Kfun` Star)

          p = IsIn "Num" (TAp tMaybe tv_a1)

          Right res = evalLogger $ toHnf ce p
       in res `shouldSatisfy` any (\(IsIn name _) -> name == "Num")


