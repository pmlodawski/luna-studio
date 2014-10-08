module Test.Luna.Typechecker.TypeclassesSpec (spec) where


import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.ClassID
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
      evalLogger (super initialEnv (ClassID "nothing")) `shouldSatisfy` isLeft
  describe "mguPred" $
    it "checks if classes match" $ do
      let res = evalLogger $ mguPred (IsIn (ClassID "YEP") (TGen 0)) (IsIn (ClassID "NOPE") (TGen 0))
      res `shouldSatisfy` isLeft
  describe "addClass" $ do
    it "checks if class was defined twice" $
      evalLogger (( addClass (ClassID "Eq") []
                <:> addClass (ClassID "Eq") []
              ) initialEnv) `shouldSatisfy` isLeft
    it "checks if class has a known superclass" $
      evalLogger (( addClass (ClassID "Eq") []
                <:> addClass (ClassID "Ord") [ClassID "EqNOPE"]
              ) initialEnv) `shouldSatisfy` isLeft
  describe "addInstance" $ do
    it "checks if instance is given for a known class" $
      evalLogger (( addClass (ClassID "Eq") []
                <:> addInst [] (IsIn (ClassID "EqNOPE") tBool)
              ) initialEnv) `shouldSatisfy` isLeft
    it "checks for overlapping instances (trivial)" $
      evalLogger (( addClass (ClassID "Eq") []
                <:> addInst [] (IsIn (ClassID "Eq") tBool)
                <:> addInst [] (IsIn (ClassID "Eq") tBool)
                 ) initialEnv) `shouldSatisfy` isLeft
    it "checks for overlapping instances (slightly non-trivial)" $
      evalLogger (( addClass (ClassID "Eq") []
                <:> addInst [IsIn (ClassID "Eq") tBool]                   (IsIn (ClassID "Eq") (list tBool))
                <:> addInst [IsIn (ClassID "Eq") (TVar $ Tyvar (TID "a") Star)] (IsIn (ClassID "Eq") (list (TVar $ Tyvar (TID "a") Star)))
                 ) initialEnv) `shouldSatisfy` isLeft
    it "checks for overlapping instances (another slightly non-trivial)" $
      evalLogger (( addClass (ClassID "Len") []
                <:> addInst [IsIn (ClassID "Len") (TVar $ Tyvar (TID "a") Star)] (IsIn (ClassID "Len") (list (TVar $ Tyvar (TID "a") Star)))
                <:> addInst [IsIn (ClassID "Len") tInt]                    (IsIn (ClassID "Len") (list tInt))
                 ) initialEnv) `shouldSatisfy` isLeft
  describe "byInst" $
    it "works" $ do
      let Right ce = evalLogger ((  addClass (ClassID "Eq") []
                               <:> addClass (ClassID "Container") []
                               <:> addInst [] (IsIn (ClassID "Eq") tInt)
                               <:> addInst [] (IsIn (ClassID "Container") tList)
                               <:> addInst [ IsIn (ClassID "Eq") (TVar $ Tyvar (TID "a") Star)
                                           , IsIn (ClassID "Container") (TVar $ Tyvar (TID "c") (Kfun Star Star))
                                           ]
                                           (IsIn (ClassID "Eq") (TAp (TVar $ Tyvar (TID "c") (Kfun Star Star)) (TVar $ Tyvar (TID "a") Star)))
                                ) initialEnv)
      evalLogger (byInst ce (IsIn (ClassID "Eq") (TAp tList tInt))) `shouldBe` Right [IsIn (ClassID "Eq") tInt, IsIn (ClassID "Container") tList]

  describe "entail" $
    it "works" $ do
      let Right ce = evalLogger ((  addClass (ClassID "Eq")       []
                               <:> addClass (ClassID "Ord")      [ClassID "Eq"]
                               <:> addClass (ClassID "Num")      []
                               <:> addClass (ClassID "Real")     [ClassID "Num", ClassID "Ord"]
                               <:> addClass (ClassID "Enum")     []
                               <:> addClass (ClassID "Integral") [ClassID "Real", ClassID "Enum"]

                               <:> addClass (ClassID "Functor")     []
                               <:> addClass (ClassID "Applicative") [ClassID "Functor"]
                               <:> addInst [IsIn (ClassID "Functor") (TVar $ Tyvar (TID "f") Star), IsIn (ClassID "Ord") (TVar $ Tyvar (TID "a") Star)]
                                           (IsIn (ClassID "Ord") (TAp (TVar $ Tyvar (TID "f") Star) (TVar $ Tyvar (TID "a") Star)))
                               <:> addInst [] (IsIn (ClassID "Eq")       tInt)   <:> addInst [] (IsIn (ClassID "Eq")       tInteger)
                               <:> addInst [] (IsIn (ClassID "Ord")      tInt)   <:> addInst [] (IsIn (ClassID "Ord")      tInteger)
                               <:> addInst [] (IsIn (ClassID "Num")      tInt)   <:> addInst [] (IsIn (ClassID "Num")      tInteger)
                               <:> addInst [] (IsIn (ClassID "Real")     tInt)   <:> addInst [] (IsIn (ClassID "Real")     tInteger)
                               <:> addInst [] (IsIn (ClassID "Enum")     tInt)   <:> addInst [] (IsIn (ClassID "Enum")     tInteger)
                               <:> addInst [] (IsIn (ClassID "Integral") tInt)   <:> addInst [] (IsIn (ClassID "Integral") tInteger)
                               <:> addInst [] (IsIn (ClassID "Functor")     tList)
                               <:> addInst [] (IsIn (ClassID "Applicative") tList)
                                ) initialEnv)
          f = TVar $ Tyvar (TID "f") Star
          a = TVar $ Tyvar (TID "a") Star
          v = TVar $ Tyvar (TID "lel") Star
          p  = IsIn (ClassID "Ord") (TAp f a)

      evalLogger (entail ce [IsIn (ClassID "Functor") f, IsIn (ClassID "Ord") a]          p)              `shouldBe` Right True
      evalLogger (entail ce [                  IsIn (ClassID "Ord") a]          p)              `shouldBe` Right False
      evalLogger (entail ce [IsIn (ClassID "Functor") f              ]          p)              `shouldBe` Right False
      evalLogger (entail ce [                              ]          p)              `shouldBe` Right False
      evalLogger (entail ce [IsIn (ClassID "Integral") a, IsIn (ClassID "Applicative") f] p)              `shouldBe` Right True
      evalLogger (entail ce [IsIn (ClassID "Integral") v]                       (IsIn (ClassID "Num") v)) `shouldBe` Right True

