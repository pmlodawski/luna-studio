module Test.Luna.Typechecker.TypeclassesSpec (spec) where

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.Typeclasses
import Luna.Typechecker.AST.Type

import Test.Hspec
import Control.Exception

import Data.Maybe (isNothing)

spec :: Spec
spec = do
  describe "insts" $
    it "raises error when no superclass" $
      evaluate (super initialEnv "nothing") `shouldThrow` anyErrorCall
  describe "mguPred" $
    it "checks if classes match" $ do
      let res = mguPred (IsIn "YEP" (TGen 0)) (IsIn "NOPE" (TGen 0))
      res `shouldBe` Nothing
  describe "addClass" $ do
    it "checks if class was defined twice" $
      (     addClass "Eq" []
        <:> addClass "Eq" []
      ) initialEnv `shouldSatisfy` isNothing
    it "checks if class has a known superclass" $
      (     addClass "Eq" []
        <:> addClass "Ord" ["EqNOPE"]
      ) initialEnv `shouldSatisfy` isNothing
  describe "addInstance" $ do
    it "checks if instance is given for a known class" $
      (     addClass "Eq" []
        <:> addInst [] (IsIn "EqNOPE" tBool)
      ) initialEnv `shouldSatisfy` isNothing
    it "checks for overlapping instances (trivial)" $
      (     addClass "Eq" []
        <:> addInst [] (IsIn "Eq" tBool)
        <:> addInst [] (IsIn "Eq" tBool)
      ) initialEnv `shouldSatisfy` isNothing
    it "checks for overlapping instances (slightly non-trivial)" $
      (     addClass "Eq" []
        <:> addInst [IsIn "Eq" tBool]                   (IsIn "Eq" (list tBool))
        <:> addInst [IsIn "Eq" (TVar $ Tyvar "a" Star)] (IsIn "Eq" (list (TVar $ Tyvar "a" Star)))
      ) initialEnv `shouldSatisfy` isNothing
    it "checks for overlapping instances (another slightly non-trivial)" $ 
      (     addClass "Len" []
        <:> addInst [IsIn "Len" (TVar $ Tyvar "a" Star)] (IsIn "Len" (list (TVar $ Tyvar "a" Star)))
        <:> addInst [IsIn "Len" tInt]                    (IsIn "Len" (list tInt))
      ) initialEnv `shouldSatisfy` isNothing
  describe "byInst" $
    it "works" $ do
      let Just ce = (  addClass "Eq" []
                   <:> addClass "Container" []
                   <:> addInst [] (IsIn "Eq" tInt)
                   <:> addInst [] (IsIn "Container" tList)
                   <:> addInst [ IsIn "Eq" (TVar $ Tyvar "a" Star)
                               , IsIn "Container" (TVar $ Tyvar "c" (Kfun Star Star))
                               ]
                               (IsIn "Eq" (TAp (TVar $ Tyvar "c" (Kfun Star Star)) (TVar $ Tyvar "a" Star)))
                    ) initialEnv
      byInst ce (IsIn "Eq" (TAp tList tInt)) `shouldBe` Just [IsIn "Eq" tInt, IsIn "Container" tList]

  describe "entail" $
    it "works" $ do
      let Just ce = (  addClass "Eq"       []
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
                    ) initialEnv
          f = TVar $ Tyvar "f" Star
          a = TVar $ Tyvar "a" Star
          v = TVar $ Tyvar "lel" Star
          --ps = [ IsIn "Functor" f
               --, IsIn "Ord"     a
               --]
          p  = IsIn "Ord" (TAp f a)

      entail ce [IsIn "Functor" f, IsIn "Ord" a]          p              `shouldBe` True
      entail ce [                  IsIn "Ord" a]          p              `shouldBe` False
      entail ce [IsIn "Functor" f              ]          p              `shouldBe` False
      entail ce [                              ]          p              `shouldBe` False
      entail ce [IsIn "Integral" a, IsIn "Applicative" f] p              `shouldBe` True
      entail ce [IsIn "Integral" v]                       (IsIn "Num" v) `shouldBe` True

