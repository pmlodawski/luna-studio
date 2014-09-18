module Luna.Typechecker.Unification (
    match, mgu,
  ) where

import Luna.Typechecker.Internal.Unification (varBind)

import Luna.Typechecker.AST.Type             (Type(..))

import Luna.Typechecker.HasKind              (HasKind(..))
import Luna.Typechecker.Substitutions        ((@@),(+->),Subst,Types(..),nullSubst,merge)

mgu :: Monad m => Type -> Type -> m Subst
mgu (TAp l r) (TAp l' r')              = do s1 <- mgu l l'
                                            s2 <- mgu (apply s1 r) (apply s1 r')
                                            return (s2 @@ s1)
mgu (TVar u) t                         = varBind u t
mgu t (TVar u)                         = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu a b                                = fail $ "types do not unify: " ++ show a ++ " // " ++ show b


match :: Monad m => Type -> Type -> m Subst
match (TAp l r) (TAp l' r') = do sl <- match l l'
                                 sr <- match r r'
                                 merge sl sr
match (TVar u) t            | kind u == kind t = return (u +-> t)
match (TCon tc1) (TCon tc2) |    tc1 == tc2    = return nullSubst
match _ _                                      = fail "types do not match"
