module Test.Luna.Typechecker.UnificationSpec (spec) where

import Luna.Typechecker.AST.Kind       (Kind(..))
import Luna.Typechecker.AST.Type
import Luna.Typechecker.Substitutions
import Test.Luna.Typechecker.AST.TypeGen



import Luna.Typechecker.Unification

import Control.Exception                        (evaluate)
import Control.Applicative                      ((<$>))

import Test.Hspec
import Test.QuickCheck
import Data.Either

spec :: Spec
spec = do
  describe "mgu" $ do
    it "matches some simple `TAp`s" $ do 
      let 
          tv0 = Tyvar "a1" Star
          tt0 = TVar tv0
          tv1 = Tyvar "b1" Star
          tt1 = TVar tv1
          tv2 = Tyvar "a2" Star
          tt2 = TVar tv2
          tv3 = Tyvar "b2" Star
          tt3 = TVar tv3

          cc0 = Tycon "A1" Star
          ct0 = TCon cc0
          --cc1 = Tycon "B1" Star
          --ct1 = TCon cc1
          --cc2 = Tycon "A2" Star
          --ct2 = TCon cc2
          cc3 = Tycon "B2" Star
          ct3 = TCon cc3

          test :: Type -> Type -> Either String Subst
          test t1 t2 = case mgu t1 t2 of
                         Left  x -> x `seq` Left  x
                         Right x -> x `seq` Right x

          testPos :: Type -> Type -> [Subst -> Expectation] -> Expectation
          testPos t1 t2 ts = do let res = test t1 t2
                                res `shouldSatisfy` isRight
                                let Right subres = res
                                mapM_ ($ subres) ts

      testPos (TAp tt0 tt1) (TAp tt2 tt3) [ (`shouldContain` [(tv0,tt2)])
                                          , (`shouldContain` [(tv1,tt3)])
                                          ]
      testPos (TAp ct0 tt1) (TAp tt2 ct3) [ (`shouldContain` [(tv1,ct3)])
                                          , (`shouldContain` [(tv2,ct0)])
                                          ]
      testPos (TAp tt2 tt1) (TAp ct0 ct3) [ (`shouldContain` [(tv1,ct3)])
                                          , (`shouldContain` [(tv2,ct0)])
                                          ]

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
  describe "match" $ do
    it "matches some trivial `TAp`s" $ do
      let 
          tv0 = Tyvar "a1" Star
          tt0 = TVar tv0
          tv1 = Tyvar "b1" Star
          tt1 = TVar tv1
          tv2 = Tyvar "a2" Star
          tt2 = TVar tv2
          tv3 = Tyvar "b2" Star
          tt3 = TVar tv3

          cc0 = Tycon "A1" Star
          ct0 = TCon cc0
          cc1 = Tycon "B1" Star
          ct1 = TCon cc1
          --cc2 = Tycon "A2" Star
          --ct2 = TCon cc2
          --cc3 = Tycon "B2" Star
          --ct3 = TCon cc3

          test :: Type -> Type -> Either String Subst
          test t1 t2 = case match t1 t2 of
                         Left  x -> x `seq` Left  x
                         Right x -> x `seq` Right x
      
      test (TAp tt0 tt1) (TAp tt2 tt3) `shouldSatisfy` isRight
      let Right res = test (TAp tt0 tt1) (TAp tt2 tt3)
      res `shouldContain` [(tv0,tt2)]
      res `shouldContain` [(tv1,tt3)]

      test (TAp tt2 tt3) (TAp tt0 tt1) `shouldSatisfy` isRight
      let Right res' = test (TAp tt2 tt3) (TAp tt0 tt1)
      res' `shouldContain` [(tv2, tt0)]
      res' `shouldContain` [(tv3, tt1)]

      evaluate (match (TAp ct0 tt1) (TAp tt2 tt3) :: Either String Subst) `shouldThrow` anyErrorCall
      evaluate (match (TAp tt0 ct1) (TAp tt2 tt3) :: Either String Subst) `shouldThrow` anyErrorCall

      test (TAp tt2 tt3) (TAp ct0 tt1) `shouldSatisfy` isRight
      test (TAp tt2 tt3) (TAp tt0 ct1) `shouldSatisfy` isRight
      (match ct0 ct0 :: Either String Subst) `shouldBe` Right nullSubst



    it "some examples for property: apply u t1 == t2 for u = match t1 t2" $ do
      let t1 = TVar (Tyvar "a" Star)
          t2 = tUnit
          u = mgu t1 t2 :: Either String Subst
          tvar = Tyvar "b" Star
          tvar' = Tyvar "b" (Kfun Star Star)
          tap = TAp (TVar tvar) (TVar tvar')
          tap' = TAp (TVar tvar') (TVar tvar)
          res = match (TVar tvar) (TVar tvar') :: Either String Subst
          res' = match tap tap' :: Either String Subst
      evaluate (flip apply t1 <$> u) `shouldReturn` Right t2
      evaluate res `shouldThrow` anyErrorCall
      evaluate res' `shouldThrow` anyErrorCall
  describe "(internals)" $ do
    describe "varBind" $ do
      it "QC: ∀ (Tyvar tv): varBind tv (TVar tv) == nullSubst" $ property $
        \tyvar -> (varBind tyvar (TVar tyvar) :: Either String Subst) `shouldBe` (Right nullSubst)
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
