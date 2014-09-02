module Test.Luna.Typechecker.Internal.TypeclassesSpec (spec) where

import           Luna.Typechecker.Internal.AST.Kind         (Kind(..))
import           Luna.Typechecker.Internal.Typeclasses      (Pred(..), initialEnv, addClass, addInst, (<:>))
import           Luna.Typechecker.Internal.AST.Type         (Type(..), Tyvar(..), tBool, list,tInt)

import           Test.Hspec

spec :: Spec
spec = do
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
      