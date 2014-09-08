module Test.Luna.Typechecker.Internal.UnificationSpec (spec) where

import Luna.Typechecker.Internal.AST.Kind       (Kind(..))
import Luna.Typechecker.Internal.AST.Type
import Luna.Typechecker.Internal.Substitutions
import Test.Luna.Typechecker.Internal.AST.TypeGen

import Luna.Typechecker.Internal.Substitutions  (Subst)
import Luna.Typechecker.Internal.Substitutions  (apply)
import Luna.Typechecker.Internal.Unification

import Control.Exception                        (evaluate)
import Control.Applicative                      ((<$>))

import Test.Hspec
import Test.QuickCheck
import Data.Either

spec :: Spec
spec = do
  describe "mgu" $ do
    it "satisfies property: apply u t1 == apply u t2 for u = mgu t1 t2" $ do
      let t1 = tUnit
          t2 = TVar (Tyvar "a" Star)
          Just u = mgu t1 t2
      apply u t1 `shouldBe` apply u t2
    it "does the kind check" $ do
      let t1 = TVar (Tyvar "a" Star)
          t2 = tList
          u = mgu t1 t2 :: Either String Subst
      evaluate u `shouldThrow` anyErrorCall
    it "does the infinite-type check" $ do
      let t1 = TVar (Tyvar "a" Star)
          t2 = list t1
          u  = mgu t1 t2 :: Either String Subst
      evaluate u `shouldThrow` anyErrorCall
    it "doesn't mind matching equal consts [QC]" $ property $
      forAll (arbitrary >>= genTCon) $ \tc ->
        let u = mgu tc tc :: Either String Subst
         in u `shouldSatisfy` isRight
    it "errors when types can't be unified" $ do
      let t1  = TVar (Tyvar "a1" Star)
          t2  = TVar (Tyvar "a2" (Kfun Star Star))
          tc1 = TCon (Tycon "Bool" Star)
          tc2 = TCon (Tycon "Maybe" (Kfun Star Star))
          tg  = TGen 0
      evaluate (mgu  t1  t2  :: Either String Subst) `shouldThrow` anyErrorCall
      evaluate (mgu tc1 tc2  :: Either String Subst) `shouldThrow` anyErrorCall
      evaluate (mgu tc1  t2  :: Either String Subst) `shouldThrow` anyErrorCall
      evaluate (mgu  t1 tc2  :: Either String Subst) `shouldThrow` anyErrorCall
      evaluate (mgu tg   t1 :: Either String Subst) `shouldThrow` anyErrorCall
      evaluate (mgu tg  tc1 :: Either String Subst) `shouldThrow` anyErrorCall
      evaluate (mgu (t1 `fn` t2 `fn` t1) tc2 :: Either String Subst) `shouldThrow` anyErrorCall
  describe "match" $
    it "some examples for property: apply u t1 == t2 for u = match t1 t2" $ do
      let t1 = TVar (Tyvar "a" Star)
          t2 = tUnit
          u = mgu t1 t2 :: Either String Subst
      evaluate (flip apply t1 <$> u) `shouldReturn` Right t2
  describe "(internals)" $ do
    describe "varBind" $ do
      it "QC: ∀ (Tyvar tv): varBind tv (TVar tv) == nullSubst" $ property $
        \tv -> (varBind tv (TVar tv) :: Either String Subst) `shouldBe` (Right nullSubst)
      it "QC: ∀ (Tyvar u, Type t): u `elem` tv t, u != tv t ===>  varBind u t == ⊥" $ property $
        forAll (genTypeNogen Star) $ \t ->
          let tvt = tv t
           in length tvt > 1 ==> (varBind (head tvt) t :: Maybe Subst) `shouldBe` Nothing
      it "QC: ∀ (Tyvar tv, Type t): kind tv /= kind t  ===>  varBind tv t == ⊥" $ property $
        forAll arbitrary $ \k1 ->
        forAll arbitrary $ \k2 ->
        forAll (genTyvar k1) $ \tvar ->
        forAll (genTVar  k2) $ \ty ->
          k1 /= k2 ==> (varBind tvar ty :: Maybe Subst) `shouldBe` Nothing