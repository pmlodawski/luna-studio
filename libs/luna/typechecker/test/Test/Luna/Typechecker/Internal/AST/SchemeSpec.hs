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
    it "quantifies variables in order of appearance in `tv qt`, *not* the order of vs [QC]" $ property $ 
      let az = map (flip Tyvar Star) vs
          gs = foldr1 fn $ map TGen $ zipWith const [0..] vs
          ks = map (const Star) vs
          vs = ["a", "b", "c", "d", "e", "f"]
       in forAll (permute az) $ \azPermuted ->
            quantify azPermuted ([] :=> (foldr1 fn $ map TVar azPermuted)) `shouldBe` Forall ks ([] :=> gs)
      --in forAll (permute $ bleh) $ \tvs_permuted ->
      --  let a = ([] :=> (foldr1 fn $ map TVar tvs))
      --      b =  Forall [Star, Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2))
      --   in quantify bleh a `shouldBe` bleh
      --let tyv1 = flip Tyvar Star "a"
      --    tyv2 = flip Tyvar Star "b"
      --    tyv3 = flip Tyvar Star "c"
      --property $ forAll ()

      --quantify [tyv1, tyv2, tyv3] ([] :=> (TVar tyv3 `fn` TVar tyv1 `fn` TVar tyv2)) `shouldBe` Forall [Star, Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2))
      --quantify [tyv1, tyv2, tyv3] ([] :=> (TVar tyv2 `fn` TVar tyv1 `fn` TVar tyv3)) `shouldBe` Forall [Star, Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2))
      --quantify [tyv1, tyv2, tyv3] ([] :=> (TVar tyv1 `fn` TVar tyv3 `fn` TVar tyv2)) `shouldBe` Forall [Star, Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2))
      --quantify [tyv3, tyv1, tyv2] ([] :=> (TVar tyv3 `fn` TVar tyv1 `fn` TVar tyv2)) `shouldBe` Forall [Star, Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2))
      --quantify [tyv3, tyv1, tyv2] ([] :=> (TVar tyv2 `fn` TVar tyv1 `fn` TVar tyv3)) `shouldBe` Forall [Star, Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2))
      --quantify [tyv3, tyv1, tyv2] ([] :=> (TVar tyv1 `fn` TVar tyv3 `fn` TVar tyv2)) `shouldBe` Forall [Star, Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2))
  describe "Luna/Typechecker/Internal/AST/Scheme.hs" $ it "is" pending
