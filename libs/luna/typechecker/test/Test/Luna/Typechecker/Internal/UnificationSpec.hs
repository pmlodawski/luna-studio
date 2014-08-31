module Test.Luna.Typechecker.Internal.UnificationSpec (spec) where

import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Luna.Typechecker.Internal.Substitutions    as Sub
import           Luna.Typechecker.Internal.Substitutions    (apply)
import           Luna.Typechecker.Internal.Unification

import           Test.Hspec
import           Control.Exception                          (evaluate)

--import           Data.Either                                (isRight)

spec :: Spec
spec = do
  describe "mgu" $ do
    it "satisfies property: apply u t1 == apply u t2 for u = mgu t1 t2" $ do
      let t1 = Ty.tUnit
          t2 = Ty.TVar (Ty.Tyvar "a" Knd.Star)
          Just u = mgu t1 t2
      apply u t1 `shouldBe` apply u t2
    it "does the kind check" $ do
      let t1 = Ty.TVar (Ty.Tyvar "a" Knd.Star)
          t2 = Ty.tList
          u = mgu t1 t2 :: Either String Sub.Subst
      evaluate u `shouldThrow` errorCall "kinds do not match"
    it "does the infinite-type check" $ do
      let t1 = Ty.TVar (Ty.Tyvar "a" Knd.Star)
          t2 = Ty.list t1
          u  = mgu t1 t2 :: Either String Sub.Subst
      evaluate u `shouldThrow` anyErrorCall
  describe "match" $
    it "satisfies property: apply u t1 == t2 for u = match t1 t2" $ do
      let t1 = Ty.TVar (Ty.Tyvar "a" Knd.Star)
          t2 = Ty.tUnit
          u = mgu t1 t2 :: Either String Sub.Subst
          Right uu = u `seq` u
       --in (apply u t1) `shouldBe` t2
      --evaluate u `shouldThrow` anyException

      apply uu t1 `shouldBe` t2
