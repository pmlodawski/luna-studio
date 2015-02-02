module Luna.Typechecker.TypesAndConstraints (
    TypesAndConstraints(..), composeSubst
  ) where


import qualified Data.Map as Map

import Luna.Typechecker.Data
import Luna.Typechecker.Tools                 (without)


class TypesAndConstraints c where
    apply :: (Monad m) =>  Subst -> c -> m c
    tv    :: c -> [TVar]


instance TypesAndConstraints Predicate where
    apply s TRUE              = return TRUE
    apply s (t1 `Subsume` t2) = do t1' <- apply s t1
                                   t2' <- apply s t2
                                   return (t1' `Subsume` t2')
    tv TRUE                   = []
    tv (t1 `Subsume` t2)      = tv t1 ++ tv t2


instance TypesAndConstraints c => TypesAndConstraints [c] where
    apply s  = mapM (apply s)
    tv       = foldl f [] where
                       f z x = z ++ tv x


instance TypesAndConstraints Constraint where
    apply s (C p)            = do p' <- apply s p
                                  return (C p')
    apply s (Proj tvl p)     = do p' <- apply s p
                                  return (Proj tvl p')
    tv (C p)                 = tv p
    tv (Proj tvl p)          = without (tv p)  tvl


instance TypesAndConstraints Type where
    apply s t                = return $ applySubst s t
    tv (TV tvl)              = [tvl]
    tv (t1 `Fun` t2)         = tv t1 ++ tv t2


instance TypesAndConstraints TypeScheme where
    apply s (Poly tvl c t)     = do c' <- apply s c
                                    t' <- apply s t
                                    return (Poly tvl c' t')
    apply s (Mono t)          = do t' <- apply s t
                                   return (Mono t')
    tv (Poly tvl c t)          = without (tv t ++ tv c) tvl
    tv (Mono t)               = tv t


applySubst :: Subst -> Type -> Type
applySubst s (TV tvl) = case Map.lookup tvl (fromSubst s) of
                          Just t  -> t
                          Nothing -> TV tvl

applySubst s (t1 `Fun` t2) = let t1' = applySubst s t1
                                 t2' = applySubst s t2
                             in t1' `Fun` t2'


composeSubst :: Subst -> Subst -> Subst
composeSubst s2 s1 = let s2' = fromSubst s2
                         s1' = fromSubst s1
                     in Subst $ Map.map (applySubst s2) s1' `Map.union` s2'
