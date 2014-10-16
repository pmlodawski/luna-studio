module Luna.Typechecker.Unification (
    mgu
  ) where

import Luna.Typechecker.IDs
import Luna.Typechecker.Substitution
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Type

import Data.Monoid



mgu :: (Monad m) => Type -> Type -> LoggerT String m Subst
mgu (TAp t1 t2) (TAp s1 s2) = do ts1 <- mgu t1 s1
                                 ts2 <- mgu (apply ts1 t2) (apply ts1 s2)
                                 return (ts1 `mappend` ts2)
mgu (TConst a) (TConst b) | a == b = return mempty
mgu (TConst _) (TConst _) = err "cantunify" "blahblah"
mgu (TVar u)   t          = varBind u t
mgu t (TVar u)            = varBind u t
mgu t1 t2 = err "mgunotimplemented" ("mgu " ++ show t1 ++ " <> " ++ show t2)

varBind :: (Monad m) => Tyvar -> Type -> LoggerT String m Subst
varBind u t | t == TVar u        = return mempty
            | u `elem` ftv t     = err "infinitetype" "blehbleh"
            | otherwise          = return (Subst [(u, t)])