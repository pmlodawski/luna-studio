module Luna.Typechecker.TypesAndConstraints (
    TypesAndConstraints(..), composeSubst, addSubst
  ) where


import            Flowbox.Prelude                   hiding (without)
import            Data.Sequence                     as Seq
import qualified  Data.Map.IntConvertibleMap        as ICMap

import            Luna.Typechecker.Data
import            Luna.Typechecker.Inference.Class  (StageTypecheckerPass)
import            Luna.Typechecker.Tools            (without)


class TypesAndConstraints c where
    apply :: (Monad m) =>  Subst -> c -> StageTypecheckerPass m c
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
    apply s t                = return $ applySubst (Seq.viewl $ fromSubst s) t
    tv (TV tvl)              = [tvl]
    tv (t1 `Fun` t2)         = tv t1 ++ tv t2
    tv (Record fields)       = foldr vars [] fields where
      vars (f,t) result = tv t ++ result


instance TypesAndConstraints TypeScheme where
    apply s (Poly tvl c t)     = do c' <- apply s c
                                    t' <- apply s t
                                    return (Poly tvl c' t')
    apply s (Mono t)          = do t' <- apply s t
                                   return (Mono t')
    tv (Poly tvl c t)          = without (tv t ++ tv c) tvl
    tv (Mono t)               = tv t


applySubst :: ViewL (TVar, Type) -> Type -> Type
applySubst EmptyL typeV = typeV
applySubst ((var, typeV) Seq.:< rest) t@(TV tvl) | var == tvl = applySubst restView typeV
                                                 | otherwise = applySubst restView t
                                                 where restView = Seq.viewl rest

applySubst s (t1 `Fun` t2) = let t1' = applySubst s t1
                                 t2' = applySubst s t2
                             in t1' `Fun` t2'

applySubst s (Record fields) = Record $ map (\(f, t) -> (f, applySubst s t)) fields


composeSubst :: Subst -> Subst -> Subst
composeSubst (Subst s2) (Subst s1) = Subst $ s1 Seq.>< s2


addSubst :: Subst -> (TVar, Type) -> Subst
addSubst (Subst s) val = Subst $ s Seq.|> val
