module Luna.Typechecker.Unification (
    mgu
  ) where

import Luna.Typechecker.Substitution
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Type



mgu :: (Monad m) => Type -> Type -> LoggerT String m Subst
mgu _ _ = err "a" "b"