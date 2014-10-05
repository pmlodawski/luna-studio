{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Luna.Typechecker.AST.TypeGen where


import qualified Luna.Typechecker.HasKind       as HKd
import           Luna.Typechecker.Typeclasses      (Pred(..))
import qualified Luna.Typechecker.Substitutions as Sub

import           Luna.Typechecker.AST.Type      (Type(..), Tyvar(..), Tycon(..))
import           Luna.Typechecker.AST.Kind      (Kind(..))
import           Luna.Typechecker.AST.TID       (enumTID, TID(..))

import           Luna.Typechecker.Internal.Logger

import           Control.Applicative                     ((<$>), (<*>))
import           Control.Monad                           (liftM2)

import           Test.QuickCheck


instance Arbitrary TID where
  arbitrary = TID <$> arbitrary


instance Arbitrary Kind where
  arbitrary = sized genKind
    where genKind n | n < 5 = return Star
          genKind n = do
            let n' = n `div` 2
            x1 <- choose (0,n' - 1)
            let x2 = n' - x1 - 1
            liftM2 Kfun (genKind x1) (genKind x2)
  shrink (Star) = []
  shrink (Kfun k1 k2) = [k1, k2, Kfun k1 k1, Kfun k2 k2] ++ (flip Kfun k2 <$> shrink k1) ++ (Kfun k1 <$> shrink k2)

instance Arbitrary Type where
  arbitrary = arbitrary >>= genType
  shrink (TVar t) = TVar <$> shrink t
  shrink (TCon t) = TCon <$> shrink t
  shrink (TAp t1 t2) = TAp <$> shrink t1 <*> shrink t2
  shrink (TGen n) = TGen <$> shrink n

instance Arbitrary Tyvar where
  arbitrary = arbitrary >>= genTyvar
  shrink (Tyvar n kind) = map (Tyvar n) (shrink kind)

instance Arbitrary Tycon where
  arbitrary = arbitrary >>= genTycon
  shrink (Tycon n kind) = map (Tycon n) (shrink kind)

instance Arbitrary Pred where
  arbitrary = liftM2 IsIn arbitrary arbitrary
  shrink (IsIn tid t) = (IsIn tid <$> shrink t) ++ (IsIn <$> shrink tid <*> [t]) ++ (IsIn <$> shrink tid <*> shrink t)


tidTC :: Int -> TID
tidTC i = TID ("tc_"++str)
  where (TID str) = enumTID i

tidTV :: Int -> TID
tidTV i = TID ("tv_"++str)
  where (TID str) = enumTID i


genPredNogen :: Kind -> Gen Pred
genPredNogen k = liftM2 IsIn arbitrary (genTypeNogen k)

genTypeNogen  :: Kind -> Gen Type
genTypeNogen k = frequency [(3, genTVar k), (3, genTCon k), (2, genTAp')]
  where genTAp' = do k2 <- arbitrary
                     t1 <- genTypeNogen (Kfun k2 k)
                     t2 <- genTypeNogen k
                     return $ TAp t1 t2

genType :: Kind -> Gen Type
genType k = frequency [(3, genTVar k), (3, genTCon k), (1, genTGen), (2, genTAp k)]

genTVar :: Kind -> Gen Type
genTVar k = TVar <$> genTyvar k

genTCon :: Kind -> Gen Type
genTCon k = do tid <- tidTC . getPositive <$> arbitrary
               return (TCon $ Tycon tid k)

genTGen :: Gen Type
genTGen = TGen <$> arbitrary

genTAp :: Kind -> Gen Type
genTAp k = do k2 <- arbitrary
              t1 <- genType (Kfun k2 k)
              t2 <- genType k
              return $ TAp t1 t2

genTyvar :: Kind -> Gen Tyvar
genTyvar k = do tid <- tidTV . getPositive <$> arbitrary
                return (Tyvar tid k)

genTycon :: Kind -> Gen Tycon
genTycon k = do tid <- tidTC . getPositive <$> arbitrary
                return (Tycon tid k)

genSubst :: [Tyvar] -> Gen Sub.Subst
genSubst []  = return Sub.nullSubst
genSubst tvs = do t <- elements tvs
                  let Right k = evalLogger $ HKd.kind t
                  ty <- genType k `suchThat` (notElem t . Sub.tv)
                  return (t Sub.+-> ty)


