module Luna.Typechecker.Internal.Unification (
    varBind
  ) where

import Luna.Typechecker.AST.Type      (Type(..),Tyvar)
import Luna.Typechecker.HasKind       (HasKind(..))
import Luna.Typechecker.Substitutions ((+->),Subst,Types(..),nullSubst)

import Luna.Typechecker.Internal.Logger

varBind :: (Monad m) => Tyvar -> Type -> TCLoggerT m Subst
varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = throwError "occurs check fail (can't build infinite type)"
            | otherwise        = do ku <- kind u
                                    kt <- kind t
                                    if ku /= kt
                                      then throwError "kinds do not match"
                                      else return (u +-> t)