module Luna.Typechecker.Unification (
    match, mgu,
  ) where

import Luna.Typechecker.HasKind              (HasKind(..))
import Luna.Typechecker.Substitutions        ((@@),(+->),Subst,Types(..),nullSubst,merge)

import Luna.Typechecker.AST.Type             (Type(..))

import Luna.Typechecker.Internal.Logger
import Luna.Typechecker.Internal.Unification (varBind)


mgu :: (Monad m) => Type -> Type -> TCLoggerT m Subst
mgu (TAp l r) (TAp l' r')              = do s1 <- mgu l l'
                                            s2 <- mgu (apply s1 r) (apply s1 r')
                                            return (s2 @@ s1)
mgu (TVar u) t                         = varBind u t
mgu t (TVar u)                         = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu a b                                = throwError $ "types do not unify: " ++ show a ++ " // " ++ show b


match :: (Monad m) => Type -> Type -> TCLoggerT m Subst
match (TAp l r) (TAp l' r') = do sl <- match l l'
                                 sr <- match r r'
                                 merge sl sr
match (TVar u) t                               = do ku <- kind u
                                                    kt <- kind t
                                                    if ku == kt
                                                      then return (u +-> t)
                                                      else throwError "types do not match"
match (TCon tc1) (TCon tc2) |    tc1 == tc2    = return nullSubst
match _ _                                      = throwError "types do not match"


