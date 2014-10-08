module Test.Luna.Typechecker.ContextReductionSpec (spec) where


import Luna.Typechecker.ContextReduction
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.ClassID
import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Control.Applicative

import Data.Either                      (isLeft)

import Test.Hspec


spec :: Spec
spec = do
  describe "inHnf" $
    it "verifies the result" $ do
      evalLogger (inHnf (IsIn (ClassID "anything") (TVar $ Tyvar (TID "a") Star))                                ) `shouldBe` Right True
      evalLogger (inHnf (IsIn (ClassID "anything") (TCon $ Tycon (TID "Int") Star))                              ) `shouldBe` Right False
      evalLogger (inHnf (IsIn (ClassID "anything") (list (TCon $ Tycon (TID "Int") Star)))                       ) `shouldBe` Right False
      evalLogger (inHnf (IsIn (ClassID "anything") (list (TVar $ Tyvar (TID "a") Star)))                         ) `shouldBe` Right False
      evalLogger (inHnf (IsIn (ClassID "anything") (TAp (TVar $ Tyvar (TID "m") Star) (TCon $ Tycon (TID "Int") Star)))) `shouldBe` Right True
      evalLogger (inHnf (IsIn (ClassID "anything") (TGen 0))                                               ) `shouldSatisfy` isLeft
  describe "toHnf" $ do
    it "works for bad input" $ do
      let Right ce = evalLogger ((     addClass (ClassID "Eq") (ClassID <$> [])
                                <:> addInst [] (IsIn (ClassID "Eq") tBool))
                              initialEnv)
          res = evalLogger (toHnf initialEnv (IsIn (ClassID "anything") (TCon $ Tycon (TID "Int") Star)))
          res2 = evalLogger (toHnf ce (IsIn (ClassID "Eq") tBool))
          res3 = evalLogger (toHnf ce (IsIn (ClassID "Eq") tInt))
      res `shouldSatisfy` isLeft
      res2 `shouldBe` Right []
      res3 `shouldSatisfy` isLeft

    it "covers nested predicates" $
      let Right ce = evalLogger ((  addClass (ClassID "Eq") (ClassID <$> [])
                         <:> addClass (ClassID "Ord") (ClassID <$> ["Eq"])
                         <:> addClass (ClassID "Num") (ClassID <$> [])
                         <:> addClass (ClassID "Real") (ClassID <$> ["Num", "Ord"])
                         <:> addClass (ClassID "Enum") (ClassID <$> [])
                         <:> addClass (ClassID "Integral") (ClassID <$> ["Real", "Enum"])
                         <:> addClass (ClassID "Fractional") (ClassID <$> ["Num"])
                         <:> addClass (ClassID "Functor") (ClassID <$> [])
                         <:> addInst [] (IsIn (ClassID "Eq") tInt)             <:> addInst [] (IsIn (ClassID "Eq") tInteger)       <:> addInst [] (IsIn (ClassID "Eq") tDouble)   <:> addInst [] (IsIn (ClassID "Eq") tFloat)
                         <:> addInst [] (IsIn (ClassID "Ord") tInt)            <:> addInst [] (IsIn (ClassID "Ord") tInteger)      <:> addInst [] (IsIn (ClassID "Ord") tDouble)  <:> addInst [] (IsIn (ClassID "Ord") tFloat)
                         <:> addInst [] (IsIn (ClassID "Num") tInt)            <:> addInst [] (IsIn (ClassID "Num") tInteger)      <:> addInst [] (IsIn (ClassID "Num") tDouble)  <:> addInst [] (IsIn (ClassID "Num") tFloat)
                         <:> addInst [] (IsIn (ClassID "Real") tInt)           <:> addInst [] (IsIn (ClassID "Real") tInteger)     <:> addInst [] (IsIn (ClassID "Real") tDouble) <:> addInst [] (IsIn (ClassID "Real") tFloat)
                         <:> addInst [] (IsIn (ClassID "Enum") tInt)           <:> addInst [] (IsIn (ClassID "Enum") tInteger)
                         <:> addInst [] (IsIn (ClassID "Integral") tInt)       <:> addInst [] (IsIn (ClassID "Integral") tInteger)
                         <:> addInst [] (IsIn (ClassID "Fractional") tDouble)  <:> addInst [] (IsIn (ClassID "Fractional") tFloat)
                         <:> addInst [] (IsIn (ClassID "Functor") tList)       <:> addInst [] (IsIn (ClassID "Functor") tMaybe)
                         <:> addInst [IsIn (ClassID "Functor") tv_f2, IsIn (ClassID "Num") tv_a1] (IsIn (ClassID "Num") (TAp tv_f2 tv_a1)) -- nonsense, I know
                         <:> addInst [IsIn (ClassID "Functor") tv_f2, IsIn (ClassID "Ord") tv_a1]
                                     (IsIn (ClassID "Ord") (TAp tv_f2 tv_a1))
                          ) initialEnv)

          tMaybe = TCon $ Tycon (TID "Maybe") $ Star `Kfun` Star

          tv_a1 = TVar $ Tyvar (TID "a") Star
          tv_f2 = TVar $ Tyvar (TID "f") (Star `Kfun` Star)

          p = IsIn (ClassID "Num") (TAp tMaybe tv_a1)

          Right res = evalLogger $ toHnf ce p
       in res `shouldSatisfy` any (\(IsIn (ClassID name) _) -> name == "Num")


