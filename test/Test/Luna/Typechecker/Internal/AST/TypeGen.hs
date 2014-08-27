module Test.Luna.Typechecker.Internal.AST.TypeGen (
    genType, genTVar, genTCon, genTAp, genTGen, genTyvar, genSubst
  ) where

import           Luna.Typechecker.Internal.AST.Type      (Type(..), Tyvar(..), Tycon(..), list, tInt)
import           Luna.Typechecker.Internal.AST.Kind      (Kind(..))
import           Luna.Typechecker.Internal.AST.TID       (enumTID, TID)

import qualified Luna.Typechecker.Internal.HasKind       as HKd
import qualified Luna.Typechecker.Internal.Substitutions as Sub

import           Control.Applicative                     ((<$>))
import           Control.Monad                           (liftM, liftM2)

import           Test.QuickCheck



instance Arbitrary Kind where
  arbitrary = sized genKind
    where genKind n | n < 5 = return Star
          genKind n = do
            let n' = n `div` 2
            x1 <- choose (0,n' - 1)
            let x2 = n' - x1 - 1
            liftM2 Kfun (genKind x1) (genKind x2)
  
instance Arbitrary Type where
  arbitrary = arbitrary >>= genType

instance Arbitrary Tyvar where
  arbitrary = arbitrary >>= genTyvar

instance Arbitrary Tycon where
  arbitrary = arbitrary >>= genTycon


tid_tc :: Int -> TID
tid_tc = ("tc_"++) . enumTID

tid_tv :: Int -> TID
tid_tv = ("tv_"++) . enumTID


genType :: Kind -> Gen Type
genType k = frequency [(3, genTVar k), (3, genTCon k), (1, genTGen), (2, genTAp k)]

genTVar k = TVar <$> genTyvar k

genTCon k = do tid <- tid_tc . getPositive <$> arbitrary
               return (TCon $ Tycon tid k)

genTGen = TGen <$> arbitrary

genTAp k = do k2 <- arbitrary
              t1 <- genType (Kfun k2 k)
              t2 <- genType k
              return $ TAp t1 t2


genTyvar k = do tid <- tid_tv . getPositive <$> arbitrary
                return (Tyvar tid k)

genTycon k = do tid <- tid_tc . getPositive <$> arbitrary
                return (Tycon tid k)

genSubst :: [Tyvar] -> Gen Sub.Subst
genSubst []  = return Sub.nullSubst
genSubst tvs = do t <- elements tvs
                  let k = HKd.kind t
                  ty <- genType k
                  return $ (t Sub.+-> ty)