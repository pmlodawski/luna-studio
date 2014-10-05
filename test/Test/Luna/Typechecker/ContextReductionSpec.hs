module Test.Luna.Typechecker.ContextReductionSpec (spec) where


import Luna.Typechecker.ContextReduction
import Luna.Typechecker.Typeclasses

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
      evalLogger (inHnf (IsIn (TID "anything") (TVar $ Tyvar (TID "a") Star))                                ) `shouldBe` Right True
      evalLogger (inHnf (IsIn (TID "anything") (TCon $ Tycon (TID "Int") Star))                              ) `shouldBe` Right False
      evalLogger (inHnf (IsIn (TID "anything") (list (TCon $ Tycon (TID "Int") Star)))                       ) `shouldBe` Right False
      evalLogger (inHnf (IsIn (TID "anything") (list (TVar $ Tyvar (TID "a") Star)))                         ) `shouldBe` Right False
      evalLogger (inHnf (IsIn (TID "anything") (TAp (TVar $ Tyvar (TID "m") Star) (TCon $ Tycon (TID "Int") Star)))) `shouldBe` Right True
      evalLogger (inHnf (IsIn (TID "anything") (TGen 0))                                               ) `shouldSatisfy` isLeft
  describe "toHnf" $ do
    it "works for bad input" $ do
      let Right ce = evalLogger ((     addClass (TID "Eq") (TID <$> [])
                                <:> addInst [] (IsIn (TID "Eq") tBool))
                              initialEnv)
          res = evalLogger (toHnf initialEnv (IsIn (TID "anything") (TCon $ Tycon (TID "Int") Star)))
          res2 = evalLogger (toHnf ce (IsIn (TID "Eq") tBool))
          res3 = evalLogger (toHnf ce (IsIn (TID "Eq") tInt))
      res `shouldSatisfy` isLeft
      res2 `shouldBe` Right []
      res3 `shouldSatisfy` isLeft

    it "covers nested predicates" $
      let Right ce = evalLogger ((  addClass (TID "Eq") (TID <$> [])
                         <:> addClass (TID "Ord") (TID <$> ["Eq"])
                         <:> addClass (TID "Num") (TID <$> [])
                         <:> addClass (TID "Real") (TID <$> ["Num", "Ord"])
                         <:> addClass (TID "Enum") (TID <$> [])
                         <:> addClass (TID "Integral") (TID <$> ["Real", "Enum"])
                         <:> addClass (TID "Fractional") (TID <$> ["Num"])
                         <:> addClass (TID "Functor") (TID <$> [])
                         <:> addInst [] (IsIn (TID "Eq") tInt)             <:> addInst [] (IsIn (TID "Eq") tInteger)       <:> addInst [] (IsIn (TID "Eq") tDouble)   <:> addInst [] (IsIn (TID "Eq") tFloat)
                         <:> addInst [] (IsIn (TID "Ord") tInt)            <:> addInst [] (IsIn (TID "Ord") tInteger)      <:> addInst [] (IsIn (TID "Ord") tDouble)  <:> addInst [] (IsIn (TID "Ord") tFloat)
                         <:> addInst [] (IsIn (TID "Num") tInt)            <:> addInst [] (IsIn (TID "Num") tInteger)      <:> addInst [] (IsIn (TID "Num") tDouble)  <:> addInst [] (IsIn (TID "Num") tFloat)
                         <:> addInst [] (IsIn (TID "Real") tInt)           <:> addInst [] (IsIn (TID "Real") tInteger)     <:> addInst [] (IsIn (TID "Real") tDouble) <:> addInst [] (IsIn (TID "Real") tFloat)
                         <:> addInst [] (IsIn (TID "Enum") tInt)           <:> addInst [] (IsIn (TID "Enum") tInteger)
                         <:> addInst [] (IsIn (TID "Integral") tInt)       <:> addInst [] (IsIn (TID "Integral") tInteger)
                         <:> addInst [] (IsIn (TID "Fractional") tDouble)  <:> addInst [] (IsIn (TID "Fractional") tFloat)
                         <:> addInst [] (IsIn (TID "Functor") tList)       <:> addInst [] (IsIn (TID "Functor") tMaybe)
                         <:> addInst [IsIn (TID "Functor") tv_f2, IsIn (TID "Num") tv_a1] (IsIn (TID "Num") (TAp tv_f2 tv_a1)) -- nonsense, I know
                         <:> addInst [IsIn (TID "Functor") tv_f2, IsIn (TID "Ord") tv_a1]
                                     (IsIn (TID "Ord") (TAp tv_f2 tv_a1))
                          ) initialEnv)

          tMaybe = TCon $ Tycon (TID "Maybe") $ Star `Kfun` Star

          tv_a1 = TVar $ Tyvar (TID "a") Star
          tv_f2 = TVar $ Tyvar (TID "f") (Star `Kfun` Star)

          p = IsIn (TID "Num") (TAp tMaybe tv_a1)

          Right res = evalLogger $ toHnf ce p
       in res `shouldSatisfy` any (\(IsIn (TID name) _) -> name == "Num")


