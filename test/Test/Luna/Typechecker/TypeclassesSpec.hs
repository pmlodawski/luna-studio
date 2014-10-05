module Test.Luna.Typechecker.TypeclassesSpec (spec) where


import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Test.Hspec

import Data.Either                      (isLeft)


spec :: Spec
spec = do
  describe "insts" $
    it "raises error when no superclass" $
      evalLogger (super initialEnv (TID "nothing")) `shouldSatisfy` isLeft
  describe "mguPred" $
    it "checks if classes match" $ do
      let res = evalLogger $ mguPred (IsIn (TID "YEP") (TGen 0)) (IsIn (TID "NOPE") (TGen 0))
      res `shouldSatisfy` isLeft
  describe "addClass" $ do
    it "checks if class was defined twice" $
      evalLogger ((     addClass (TID "Eq") []
                <:> addClass (TID "Eq") []
              ) initialEnv) `shouldSatisfy` isLeft
    it "checks if class has a known superclass" $
      evalLogger ((     addClass (TID "Eq") []
                <:> addClass (TID "Ord") [TID "EqNOPE"]
              ) initialEnv) `shouldSatisfy` isLeft
  describe "addInstance" $ do
    it "checks if instance is given for a known class" $
      evalLogger ((     addClass (TID "Eq") []
                <:> addInst [] (IsIn (TID "EqNOPE") tBool)
              ) initialEnv) `shouldSatisfy` isLeft
    it "checks for overlapping instances (trivial)" $
      evalLogger ((     addClass (TID "Eq") []
                <:> addInst [] (IsIn (TID "Eq") tBool)
                <:> addInst [] (IsIn (TID "Eq") tBool)
                 ) initialEnv) `shouldSatisfy` isLeft
    it "checks for overlapping instances (slightly non-trivial)" $
      evalLogger ((     addClass (TID "Eq") []
                <:> addInst [IsIn (TID "Eq") tBool]                   (IsIn (TID "Eq") (list tBool))
                <:> addInst [IsIn (TID "Eq") (TVar $ Tyvar (TID "a") Star)] (IsIn (TID "Eq") (list (TVar $ Tyvar (TID "a") Star)))
                 ) initialEnv) `shouldSatisfy` isLeft
    it "checks for overlapping instances (another slightly non-trivial)" $
      evalLogger ((     addClass (TID "Len") []
                <:> addInst [IsIn (TID "Len") (TVar $ Tyvar (TID "a") Star)] (IsIn (TID "Len") (list (TVar $ Tyvar (TID "a") Star)))
                <:> addInst [IsIn (TID "Len") tInt]                    (IsIn (TID "Len") (list tInt))
                 ) initialEnv) `shouldSatisfy` isLeft
  describe "byInst" $
    it "works" $ do
      let Right ce = evalLogger ((  addClass (TID "Eq") []
                               <:> addClass (TID "Container") []
                               <:> addInst [] (IsIn (TID "Eq") tInt)
                               <:> addInst [] (IsIn (TID "Container") tList)
                               <:> addInst [ IsIn (TID "Eq") (TVar $ Tyvar (TID "a") Star)
                                           , IsIn (TID "Container") (TVar $ Tyvar (TID "c") (Kfun Star Star))
                                           ]
                                           (IsIn (TID "Eq") (TAp (TVar $ Tyvar (TID "c") (Kfun Star Star)) (TVar $ Tyvar (TID "a") Star)))
                                ) initialEnv)
      evalLogger (byInst ce (IsIn (TID "Eq") (TAp tList tInt))) `shouldBe` Right [IsIn (TID "Eq") tInt, IsIn (TID "Container") tList]

  describe "entail" $
    it "works" $ do
      let Right ce = evalLogger ((  addClass (TID "Eq")       []
                               <:> addClass (TID "Ord")      [TID "Eq"]
                               <:> addClass (TID "Num")      []
                               <:> addClass (TID "Real")     [TID "Num", TID "Ord"]
                               <:> addClass (TID "Enum")     []
                               <:> addClass (TID "Integral") [TID "Real", TID "Enum"]

                               <:> addClass (TID "Functor")     []
                               <:> addClass (TID "Applicative") [TID "Functor"]
                               <:> addInst [IsIn (TID "Functor") (TVar $ Tyvar (TID "f") Star), IsIn (TID "Ord") (TVar $ Tyvar (TID "a") Star)]
                                           (IsIn (TID "Ord") (TAp (TVar $ Tyvar (TID "f") Star) (TVar $ Tyvar (TID "a") Star)))
                               <:> addInst [] (IsIn (TID "Eq")       tInt)   <:> addInst [] (IsIn (TID "Eq")       tInteger)
                               <:> addInst [] (IsIn (TID "Ord")      tInt)   <:> addInst [] (IsIn (TID "Ord")      tInteger)
                               <:> addInst [] (IsIn (TID "Num")      tInt)   <:> addInst [] (IsIn (TID "Num")      tInteger)
                               <:> addInst [] (IsIn (TID "Real")     tInt)   <:> addInst [] (IsIn (TID "Real")     tInteger)
                               <:> addInst [] (IsIn (TID "Enum")     tInt)   <:> addInst [] (IsIn (TID "Enum")     tInteger)
                               <:> addInst [] (IsIn (TID "Integral") tInt)   <:> addInst [] (IsIn (TID "Integral") tInteger)
                               <:> addInst [] (IsIn (TID "Functor")     tList)
                               <:> addInst [] (IsIn (TID "Applicative") tList)
                                ) initialEnv)
          f = TVar $ Tyvar (TID "f") Star
          a = TVar $ Tyvar (TID "a") Star
          v = TVar $ Tyvar (TID "lel") Star
          p  = IsIn (TID "Ord") (TAp f a)

      evalLogger (entail ce [IsIn (TID "Functor") f, IsIn (TID "Ord") a]          p)              `shouldBe` Right True
      evalLogger (entail ce [                  IsIn (TID "Ord") a]          p)              `shouldBe` Right False
      evalLogger (entail ce [IsIn (TID "Functor") f              ]          p)              `shouldBe` Right False
      evalLogger (entail ce [                              ]          p)              `shouldBe` Right False
      evalLogger (entail ce [IsIn (TID "Integral") a, IsIn (TID "Applicative") f] p)              `shouldBe` Right True
      evalLogger (entail ce [IsIn (TID "Integral") v]                       (IsIn (TID "Num") v)) `shouldBe` Right True

