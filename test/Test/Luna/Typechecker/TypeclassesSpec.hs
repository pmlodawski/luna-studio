module Test.Luna.Typechecker.TypeclassesSpec (spec) where


import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Test.Hspec

import Control.Exception

import Data.Either                      (isLeft)


spec :: Spec
spec = do
  describe "insts" $
    it "raises error when no superclass" $
      evalLogger (super initialEnv "nothing") `shouldSatisfy` isLeft
  describe "mguPred" $
    it "checks if classes match" $ do
      let res = evalLogger $ mguPred (IsIn "YEP" (TGen 0)) (IsIn "NOPE" (TGen 0))
      res `shouldSatisfy` isLeft
  describe "addClass" $ do
    it "checks if class was defined twice" $
      evalLogger ((     addClass "Eq" []
                <:> addClass "Eq" []
              ) initialEnv) `shouldSatisfy` isLeft
    it "checks if class has a known superclass" $
      evalLogger ((     addClass "Eq" []
                <:> addClass "Ord" ["EqNOPE"]
              ) initialEnv) `shouldSatisfy` isLeft
  describe "addInstance" $ do
    it "checks if instance is given for a known class" $
      evalLogger ((     addClass "Eq" []
                <:> addInst [] (IsIn "EqNOPE" tBool)
              ) initialEnv) `shouldSatisfy` isLeft
    it "checks for overlapping instances (trivial)" $
      evalLogger ((     addClass "Eq" []
                <:> addInst [] (IsIn "Eq" tBool)
                <:> addInst [] (IsIn "Eq" tBool)
                 ) initialEnv) `shouldSatisfy` isLeft
    it "checks for overlapping instances (slightly non-trivial)" $
      evalLogger ((     addClass "Eq" []
                <:> addInst [IsIn "Eq" tBool]                   (IsIn "Eq" (list tBool))
                <:> addInst [IsIn "Eq" (TVar $ Tyvar "a" Star)] (IsIn "Eq" (list (TVar $ Tyvar "a" Star)))
                 ) initialEnv) `shouldSatisfy` isLeft
    it "checks for overlapping instances (another slightly non-trivial)" $ 
      evalLogger ((     addClass "Len" []
                <:> addInst [IsIn "Len" (TVar $ Tyvar "a" Star)] (IsIn "Len" (list (TVar $ Tyvar "a" Star)))
                <:> addInst [IsIn "Len" tInt]                    (IsIn "Len" (list tInt))
                 ) initialEnv) `shouldSatisfy` isLeft
  describe "byInst" $
    it "works" $ do
      let Right ce = evalLogger ((  addClass "Eq" []
                               <:> addClass "Container" []
                               <:> addInst [] (IsIn "Eq" tInt)
                               <:> addInst [] (IsIn "Container" tList)
                               <:> addInst [ IsIn "Eq" (TVar $ Tyvar "a" Star)
                                           , IsIn "Container" (TVar $ Tyvar "c" (Kfun Star Star))
                                           ]
                                           (IsIn "Eq" (TAp (TVar $ Tyvar "c" (Kfun Star Star)) (TVar $ Tyvar "a" Star)))
                                ) initialEnv)
      evalLogger (byInst ce (IsIn "Eq" (TAp tList tInt))) `shouldBe` Right [IsIn "Eq" tInt, IsIn "Container" tList]

  describe "entail" $
    it "works" $ do
      let Right ce = evalLogger ((  addClass "Eq"       []
                               <:> addClass "Ord"      ["Eq"]
                               <:> addClass "Num"      []
                               <:> addClass "Real"     ["Num", "Ord"]
                               <:> addClass "Enum"     []
                               <:> addClass "Integral" ["Real", "Enum"]

                               <:> addClass "Functor"     []
                               <:> addClass "Applicative" ["Functor"]
                               <:> addInst [IsIn "Functor" (TVar $ Tyvar "f" Star), IsIn "Ord" (TVar $ Tyvar "a" Star)]
                                           (IsIn "Ord" (TAp (TVar $ Tyvar "f" Star) (TVar $ Tyvar "a" Star)))
                               <:> addInst [] (IsIn "Eq"       tInt)   <:> addInst [] (IsIn "Eq"       tInteger)
                               <:> addInst [] (IsIn "Ord"      tInt)   <:> addInst [] (IsIn "Ord"      tInteger)
                               <:> addInst [] (IsIn "Num"      tInt)   <:> addInst [] (IsIn "Num"      tInteger)
                               <:> addInst [] (IsIn "Real"     tInt)   <:> addInst [] (IsIn "Real"     tInteger)
                               <:> addInst [] (IsIn "Enum"     tInt)   <:> addInst [] (IsIn "Enum"     tInteger)
                               <:> addInst [] (IsIn "Integral" tInt)   <:> addInst [] (IsIn "Integral" tInteger)
                               <:> addInst [] (IsIn "Functor"     tList)
                               <:> addInst [] (IsIn "Applicative" tList)
                                ) initialEnv)
          f = TVar $ Tyvar "f" Star
          a = TVar $ Tyvar "a" Star
          v = TVar $ Tyvar "lel" Star
          p  = IsIn "Ord" (TAp f a)

      evalLogger (entail ce [IsIn "Functor" f, IsIn "Ord" a]          p)              `shouldBe` Right True
      evalLogger (entail ce [                  IsIn "Ord" a]          p)              `shouldBe` Right False
      evalLogger (entail ce [IsIn "Functor" f              ]          p)              `shouldBe` Right False
      evalLogger (entail ce [                              ]          p)              `shouldBe` Right False
      evalLogger (entail ce [IsIn "Integral" a, IsIn "Applicative" f] p)              `shouldBe` Right True
      evalLogger (entail ce [IsIn "Integral" v]                       (IsIn "Num" v)) `shouldBe` Right True

