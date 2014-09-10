{-# LANGUAGE ScopedTypeVariables #-}

module Test.Luna.Typechecker.Internal.AST.SchemeSpec (spec) where

--import Luna.Typechecker.Internal.AST.Alternatives
--import Luna.Typechecker.Internal.AST.Common
--import Luna.Typechecker.Internal.AST.Expr
import Luna.Typechecker.Internal.AST.Kind
--import Luna.Typechecker.Internal.AST.Lit
--import Luna.Typechecker.Internal.AST.Module
--import Luna.Typechecker.Internal.AST.Pat
import Luna.Typechecker.Internal.AST.Scheme
--import Luna.Typechecker.Internal.AST.TID
import Luna.Typechecker.Internal.AST.Type
import Test.Luna.Typechecker.Internal.AST.TypeGen


--import Luna.Typechecker.Internal.Ambiguity
--import Luna.Typechecker.Internal.Assumptions
--import Luna.Typechecker.Internal.BindingGroups
--import Luna.Typechecker.Internal.ContextReduction
--import Luna.Typechecker.Internal.HasKind
--import Luna.Typechecker.Internal.Substitutions
--import Luna.Typechecker.Internal.TIMonad
import Luna.Typechecker.Internal.Typeclasses
--import Luna.Typechecker.Internal.TypeInference
--import Luna.Typechecker.Internal.Unification
--import Luna.Typechecker

import Test.Hspec
import Test.QuickCheck
import Text.Printf
import Debug.Trace

import Control.Monad.Trans.State.Strict
import Data.Array.ST
import Control.Monad.ST


-- |Knuth shuffle.
permute :: [a] -> Gen [a]
permute xs = do swaps <- mapM mkSwapTuple [nd,nd-1..1]
                return (runST $ permute' xs swaps)
  where nd = length xs - 1
        mkSwapTuple i = do k <- choose (0,i)
                           return (k,i)
        permute' (xs :: [a]) swaps = do arr <- newListArray (0,nd) xs :: ST s (STArray s Int a)
                                        mapM_ (swap arr) swaps
                                        getElems arr
        swap arr (i,j) = do a <- readArray arr i
                            b <- readArray arr j
                            writeArray arr i b
                            writeArray arr j a
  

spec :: Spec
spec = do
  describe "quantify" $ do
    it "quantifies variables in order of appearance in `tv qt`, *not* the order of `vs` [QC]" $ property $ 
      let az = map (flip Tyvar Star) vs
          az' = map (flip Tyvar Star) vs'
          az'' = map (flip Tyvar Star) vs''
          gs = foldr1 fn $ map TGen $ zipWith const [0..] vs
          gs' = foldr1 fn $ ((map TGen $ zipWith const [0..] vs) ++ [TVar $ Tyvar "y" Star])
          ks = map (const Star) vs
          vs = ["a", "b", "c", "d", "e", "f"]
          vs' = ["z"] -- variable to quantify that does not exist
          vs'' = ["y"] -- the variable that is not quantified
       in forAll (permute az) $ \azPermuted ->
            quantify (azPermuted++az') ([] :=> (foldr1 fn $ map TVar (azPermuted++az''))) `shouldBe` Forall ks ([] :=> gs')
  describe "(coverage booster)" $ do
    it "instance Show Scheme" $ do
      let sch1 = Forall [] ([] :=> t)
          sch2 = Forall ks ([] :=> tk)
          sch3 = Forall [] (ps :=> t)
          sch4 = Forall ks (psk :=> tk)
          ks = [Star]
          ps = [IsIn "Integral" t]
          psk = [IsIn "Integral" tk]
          t = TVar $ Tyvar "a" Star
          tk = TGen 0
      length (concatMap show [sch1, sch2, sch3, sch4]) `shouldSatisfy` (>0)