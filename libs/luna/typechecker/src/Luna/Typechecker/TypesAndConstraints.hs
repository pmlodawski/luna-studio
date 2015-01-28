module Luna.Typechecker.TypesAndConstraints (
    TypesAndConstraints(..)
  ) where


import Luna.Typechecker.Data
import Luna.Typechecker.Inference.Class (StageTypecheckerPass)
import Luna.Typechecker.Tools                 (without)



class TypesAndConstraints c where
  apply :: (Monad m) =>  Subst -> c -> StageTypecheckerPass m c
  tv    :: c -> [TVar]


instance TypesAndConstraints Predicate where
  apply s TRUE              = return TRUE
  apply s (t1 `Subsume` t2) = do t1' <- apply s t1
                                 t2' <- apply s t2
                                 return (t1' `Subsume` t2')
  apply s (Reckind t l t')  = do t1 <- apply s t
                                 t1' <- apply s t'
                                 return (Reckind t1 l t1')
  tv TRUE                   = []
  tv (t1 `Subsume` t2)      = tv t1 ++ tv t2
  tv (Reckind t l t')       = tv t ++ tv t'


instance TypesAndConstraints c => TypesAndConstraints [c] where
  apply s  = mapM (apply s)
  tv       = foldl f [] where
                       f z x = z ++ tv x


instance TypesAndConstraints Constraint where
     apply s (C p)            = do p' <- apply s p
                                   return (C p')
     apply s (Proj tvl p)      = do p' <- apply s p
                                    return (Proj tvl p')
     tv (C p)                 = tv p
     tv (Proj tvl p)           = without (tv p)  tvl


instance TypesAndConstraints Type where
  apply s (TV tvl)           = case lookup tvl s of
                                  Just t  -> return t
                                  Nothing -> return (TV tvl)
  apply s (t1 `Fun` t2)     = do t1' <- apply s t1
                                 t2' <- apply s t2
                                 return (t1' `Fun` t2')
  apply s (Record [])        = return (Record [])
  apply s (Record ((l,t):f)) = do f' <- apply s (Record f)
                                  case f' of
                                    Record f'' -> do t' <- apply s t
                                                     return (Record ((l,t'):f''))
                                    _          -> fail "apply:uncompatible types" (Record [])
                                    -- _          -> report_error "apply:uncompatible types" (Record [])
                                    -- TODO [kgdk] 28 sty 2015: fix imports
  tv (TV tvl)                = [tvl]
  tv (t1 `Fun` t2)          = tv t1 ++ tv t2
  tv (Record [])            = []
  tv (Record ((l,t):f))     = tv t ++ tv (Record f)


instance TypesAndConstraints TypeScheme where
  apply s (Poly tvl c t)     = do c' <- apply s c
                                  t' <- apply s t
                                  return (Poly tvl c' t')
  apply s (Mono t)          = do t' <- apply s t
                                 return (Mono t')
  tv (Poly tvl c t)          = without (tv t ++ tv c) tvl
  tv (Mono t)               = tv t
