{-# LANGUAGE ScopedTypeVariables #-}

module Test.Luna.Typechecker.AST.SchemeSpec (spec) where

--import Luna.Typechecker.AST.Alternatives
--import Luna.Typechecker.AST.Common
--import Luna.Typechecker.AST.Expr
import Luna.Typechecker.AST.Kind
--import Luna.Typechecker.AST.Lit
--import Luna.Typechecker.AST.Module
--import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
--import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type
--import Test.Luna.Typechecker.AST.TypeGen


--import Luna.Typechecker.Ambiguity
--import Luna.Typechecker.Assumptions
--import Luna.Typechecker.BindingGroups
--import Luna.Typechecker.ContextReduction
--import Luna.Typechecker.HasKind
--import Luna.Typechecker.Substitutions
--import Luna.Typechecker.TIMonad
import Luna.Typechecker.Typeclasses
--import Luna.Typechecker.TypeInference
--import Luna.Typechecker.Unification
--import Luna.Typechecker

import Test.Hspec
import Test.QuickCheck
--import Text.Printf
--import Debug.Trace

--import Control.Monad.Trans.State.Strict
import Data.Array.ST
import Control.Monad.ST


-- |Knuth shuffle.
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
  describe "quantify" $ do
    it "quantifies variables in order of appearance in `tv qt`, *not* the order of `vs` [QC]" $ property $ 
      let az = map (flip Tyvar Star) vs
          az' = map (flip Tyvar Star) vs'
          az'' = map (flip Tyvar Star) vs''
          --gs = foldr1 fn $ map TGen $ zipWith const [0..] vs
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
