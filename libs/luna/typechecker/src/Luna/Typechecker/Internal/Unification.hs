module Luna.Typechecker.Internal.Unification (
    varBind
  ) where

import           Luna.Typechecker.AST.Type         (Type(..),Tyvar)

import           Luna.Typechecker.HasKind          (HasKind(..))
import           Luna.Typechecker.Substitutions    ((+->),Subst,Types(..),nullSubst)



varBind :: Monad m => Tyvar -> Type -> m Subst
varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = fail "occurs check fail (can't build infinite type)"
            | kind u /= kind t = fail "kinds do not match"
            | otherwise        = return (u +-> t)