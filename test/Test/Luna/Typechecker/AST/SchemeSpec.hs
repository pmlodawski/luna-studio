{-# LANGUAGE ScopedTypeVariables #-}

module Test.Luna.Typechecker.AST.SchemeSpec (spec) where


import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Control.Applicative
import Control.Monad.ST

import Data.Array.ST

import Test.Hspec
import Test.QuickCheck


permute :: [a] -> Gen [a]
permute xs = do swaps <- mapM mkSwapTuple [nd,nd-1..1]
                return (runST $ permute' xs swaps)
  where nd = length xs - 1
        mkSwapTuple i = do k <- choose (0,i)
                           return (k,i)
        permute' (xss :: [a]) swaps = do arr <- newListArray (0,nd) xss :: ST s (STArray s Int a)
                                         mapM_ (swap arr) swaps
                                         getElems arr
        swap arr (i,j) = do a <- readArray arr i
                            b <- readArray arr j
                            writeArray arr i b
                            writeArray arr j a
 

spec :: Spec
spec = do
  describe "quantify" $
    it "quantifies variables in order of appearance in `tv qt`, *not* the order of `vs` [QC]" $ property $
      let az = map (flip Tyvar Star) vs
          az' = map (flip Tyvar Star) vs'
          az'' = map (flip Tyvar Star) vs''
          gs' = foldr1 fn (map TGen (zipWith const [0..] vs) ++ [TVar $ Tyvar (TID "y") Star])
          ks = map (const Star) vs
          vs = TID <$> ["a", "b", "c", "d", "e", "f"]
          vs' = TID <$> ["z"] -- variable to quantify that does not exist
          vs'' = TID <$> ["y"] -- the variable that is not quantified
       in forAll (permute az) $ \azPermuted ->
            evalLogger (quantify (azPermuted++az') ([] :=> foldr1 fn (map TVar (azPermuted++az'')))) `shouldBe` Right (Forall ks ([] :=> gs'))
  describe "(coverage booster)" $
    it "instance Show Scheme" $ do
      let sch1 = Forall [] ([] :=> t)
          sch2 = Forall ks ([] :=> tk)
          sch3 = Forall [] (ps :=> t)
          sch4 = Forall ks (psk :=> tk)
          ks = [Star]
          ps = [IsIn (TID "Integral") t]
          psk = [IsIn (TID "Integral") tk]
          t = TVar $ Tyvar (TID "a") Star
          tk = TGen 0
      length (concatMap show [sch1, sch2, sch3, sch4]) `shouldSatisfy` (>0)
