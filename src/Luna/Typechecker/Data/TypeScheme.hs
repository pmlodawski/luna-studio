{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Luna.Typechecker.Data.TypeScheme where


import Flowbox.Prelude
import Data.List                        (sort,nub,sortBy,intercalate,permutations)
import Control.Monad                    (zipWithM_)
import Control.Monad                    (filterM,forM)
import Data.List                        (sortBy, nub)
import Data.Ord                         (comparing)
import qualified  Data.Map.Strict             as M
import Data.Maybe

import Luna.Typechecker.AlphaEquiv
import Luna.Typechecker.Data.Constraint
import Luna.Typechecker.Data.Type
import Luna.Typechecker.Data.TVar



data TypeScheme = Mono Type
                | Poly [TVar] Constraint Type
                deriving (Show)


instance AlphaEquiv TypeScheme where
    equiv (Mono a)           (Mono b)           = equiv a b
    equiv (Poly tvs1 c1 ty1) (Poly tvs2 c2 ty2) = do
        equiv ty1 ty2
        equiv c1 c2
        -- PERFORMANCE [kgdk] 23 lut 2015:
        -- currently: O(n!)
        -- better: O(n log n) -- check if boundness & unboundness of all variables in constraint/type do match
        if  | length tvs1C < length tvs2C -> fork [zipWithM_ equiv tvs1C' tvs2C  | tvs1C' <- permutations tvs1C]
            | otherwise                   -> fork [zipWithM_ equiv tvs1C  tvs2C' | tvs2C' <- permutations tvs2C]
      where tvs1C = nub tvs1
            tvs2C = nub tvs2

    translateBtoA (Mono ty)       = Mono <$> translateBtoA ty
    translateBtoA (Poly tvs c ty) = Poly <$> mapM ttBtoA tvs <*> translateBtoA c <*> translateBtoA ty