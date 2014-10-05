module Luna.Typechecker.Internal.Unification (
    varBind
  ) where


import Luna.Typechecker.HasKind       (HasKind(..))
import Luna.Typechecker.Substitutions ((+->),Subst,Types(..),nullSubst)

import Luna.Typechecker.AST.Type      (Type(..),Tyvar)

import Luna.Typechecker.Internal.Logger

import Control.Monad                  (liftM2,when)


varBind :: (Monad m) => Tyvar -> Type -> TCLoggerT m Subst
varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = throwError "occurs check fail (can't build infinite type)"
            | otherwise        = do kindsMismatch <- liftM2 (/=) (kind u) (kind t)
                                    when kindsMismatch $ throwError "kinds do not match"
                                    return (u +-> t)
