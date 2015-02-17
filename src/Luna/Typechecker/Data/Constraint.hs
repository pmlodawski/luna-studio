module Luna.Typechecker.Data.Constraint where


import Flowbox.Prelude
import Data.List                        (sort,nub,sortBy,intercalate,permutations)
import Data.Ord                         (comparing)
import Data.Function                    (on)
import Data.Maybe                    (isNothing,isJust,fromJust)
import Control.Monad                    (zipWithM_)
import            Control.Monad               (ap, forM)
import Data.Ord                         (comparing)
import qualified  Data.Map.Strict             as M

import Data.IntSet (IntSet)

import Luna.Typechecker.AlphaEquiv
import Luna.Typechecker.Data.TVar
import Luna.Typechecker.Data.Predicate



data Constraint = C [Predicate]
                | Proj [TVar] [Predicate]
                deriving (Show)


instance Monoid Constraint where
    mempty = C [TRUE]
    mappend (C p1) (C p2)               = C (p1 ++ p2)
    mappend (C p1) (Proj tvr p2)        = Proj tvr (p1 ++ p2)
    mappend (Proj tvr p1) (C p2)        = Proj tvr (p1 ++ p2)
    mappend (Proj tv1 p1) (Proj tv2 p2) = Proj (tv1 ++ tv2) (p1 ++ p2)


true_cons :: Constraint
true_cons = C [TRUE]


instance AlphaEquiv Constraint where
    equiv (C predsL) (C predsR)
      | length predsLC < length predsRC = fork [zipWithM_ equiv predsLC' predsRC  | predsLC' <- permutations predsLC]
      | otherwise                       = fork [zipWithM_ equiv predsLC  predsRC' | predsRC' <- permutations predsRC]
      where
        predsLC = filter (/= TRUE) predsL
        predsRC = filter (/= TRUE) predsR

    equiv (C p) b = equiv (Proj [] p) b
    equiv a (C p) = equiv a (Proj [] p)

    equiv (Proj tvs1 ps1) (Proj tvs2 ps2) = do
        eq <- equiv (C ps1) (C ps2)
        -- PERFORMANCE [kgdk] 23 lut 2015:
        -- currently: O(n!)
        -- better:  O(n log n) -- check if boundness & unboundness of all variables in constraint/type do match
        if  | length tvs1C < length tvs2C -> fork [zipWithM_ equiv tvs1C' tvs2C  | tvs1C' <- permutations tvs1C]
            | otherwise                   -> fork [zipWithM_ equiv tvs1C  tvs2C' | tvs2C' <- permutations tvs2C]
      where tvs1C = nub tvs1
            tvs2C = nub tvs2

    translateBtoA (C        ps) = C    <$>                     mapM translateBtoA ps
    translateBtoA (Proj tvs ps) = Proj <$> mapM ttBtoA tvs <*> mapM translateBtoA ps
