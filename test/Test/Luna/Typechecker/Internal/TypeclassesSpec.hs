module Test.Luna.Typechecker.Internal.TypeclassesSpec (spec) where

import Luna.Typechecker.Internal.AST.Kind
import Luna.Typechecker.Internal.Typeclasses
import Luna.Typechecker.Internal.AST.Type

import Test.Hspec
import Control.Exception

spec :: Spec
spec = do
  describe "insts" $ do
    it "raises error when no superclass" $ do
      evaluate (super initialEnv "nothing") `shouldThrow` anyErrorCall
  describe "mguPred" $ do
    it "checks if classes match" $ do
      let res = mguPred (IsIn "YEP" (TGen 0)) (IsIn "NOPE" (TGen 0))
      res `shouldBe` Nothing
  describe "addClass" $ do
    it "checks if class was defined twice" $
      (     addClass "Eq" []
        <:> addClass "Eq" []
      ) initialEnv `shouldBe` Nothing
    it "checks if class has a known superclass" $
      (     addClass "Eq" []
        <:> addClass "Ord" ["EqNOPE"]
      ) initialEnv `shouldBe` Nothing
  describe "addInstance" $ do
    it "checks if instance is given for a known class" $
      (     addClass "Eq" []
        <:> addInst [] (IsIn "EqNOPE" tBool)
      ) initialEnv `shouldBe` Nothing
    it "checks for overlapping instances (trivial)" $
      (     addClass "Eq" []
        <:> addInst [] (IsIn "Eq" tBool)
        <:> addInst [] (IsIn "Eq" tBool)
      ) initialEnv `shouldBe` Nothing
    it "checks for overlapping instances (slightly non-trivial)" $
      (     addClass "Eq" []
        <:> addInst [IsIn "Eq" tBool]                   (IsIn "Eq" (list tBool))
        <:> addInst [IsIn "Eq" (TVar $ Tyvar "a" Star)] (IsIn "Eq" (list (TVar $ Tyvar "a" Star)))
      ) initialEnv `shouldBe` Nothing
    it "checks for overlapping instances (another slightly non-trivial)" $ 
      (     addClass "Len" []
        <:> addInst [IsIn "Len" (TVar $ Tyvar "a" Star)] (IsIn "Len" (list (TVar $ Tyvar "a" Star)))
        <:> addInst [IsIn "Len" tInt]                    (IsIn "Len" (list tInt))
      ) initialEnv `shouldBe` Nothing
  describe "byInst" $ do
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
