{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Luna.Typechecker.Data.TypeScheme where


import            Flowbox.Prelude

import qualified  Data.IntConvertibleSet as S

import            Luna.Typechecker.AlphaEquiv
import            Luna.Typechecker.Data.Constraint
import            Luna.Typechecker.Data.Predicate
import            Luna.Typechecker.Data.Type
import            Luna.Typechecker.Data.TVar



data TypeScheme = Mono Type
                | Poly [TVar] Constraint Type
                deriving (Show)


instance AlphaEquiv TypeScheme where
    equiv    (Mono a)              (Mono b)  = equiv a b
    equiv    (Mono ty)             y         = equiv (Poly [] (C [TRUE]) ty) y
    equiv    x                     (Mono ty) = equiv x (Poly [] (C [TRUE]) ty)
    equiv p1@(Poly tvs1 c1 ty1) p2@(Poly tvs2 c2 ty2)
      | S.size free1  /= S.size free2  = notAlphaEquivalent
      | S.size quant1 /= S.size quant2 = notAlphaEquivalent
      | otherwise = do
          equiv ty1 ty2
          equiv c1 c2
          nonDeterministicEquiv (S.toList free1)  (S.toList free2)
          nonDeterministicEquiv (S.toList quant1) (S.toList quant2)
      where tvars_tvs1 = S.fromList tvs1
            free1      = (freevars c1 <> freevars ty1) `S.difference` tvars_tvs1
            quant1     = tvars_tvs1 `S.intersection` (freevars c1 <> freevars ty1)

            tvars_tvs2 = S.fromList tvs2
            free2      = (freevars c2 <> freevars ty2) `S.difference` tvars_tvs2
            quant2     = tvars_tvs2 `S.intersection` (freevars c2 <> freevars ty2)


        -- PERFORMANCE [kgdk] 23 lut 2015:
        -- currently: O(n*n!)
        -- better   : O(n!) -- expand fork&permutations, do not recalculate
        -- better   : O(n log n) -- check if boundness & unboundness of all variables in constraint/type do match


    translateBtoA (Mono ty)       = Mono <$> translateBtoA ty
    translateBtoA (Poly tvs c ty) = Poly <$> mapM ttBtoA tvs <*> translateBtoA c <*> translateBtoA ty

    freevars (Mono a)        = freevars a
    freevars (Poly tvs c ty) = (freevars c <> freevars ty) `S.difference` S.fromList tvs
