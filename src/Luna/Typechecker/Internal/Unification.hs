module Luna.Typechecker.Internal.Unification (mgu, match) where

import           Luna.Typechecker.Internal.AST.Type         (Type(..),Tyvar)

import           Luna.Typechecker.Internal.HasKind          (HasKind(..))
import           Luna.Typechecker.Internal.Substitutions    ((@@),(+->),Subst,Types(..),nullSubst,merge)


-- | Most general unifier.
mgu :: Monad m => Type -> Type -> m Subst
mgu (TAp l r) (TAp l' r')              = do s1 <- mgu l l'
                                            s2 <- mgu (apply s1 r) (apply s1 r')
                                            return (s2 @@ s1)
mgu (TVar u) t                         = varBind u t
mgu t (TVar u)                         = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu a b                                = fail $ "types do not unify: " ++ show a ++ " // " ++ show b


-- | Unify type variable with a type.
-- In practice tests are required to ensure that this is valid, including
-- an occurs check ('u `elem` tv t') and a test to ensure that the substitution is kind-preserving.
varBind :: Monad m => Tyvar -> Type -> m Subst
varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = fail "occurs check fail (can't build infinite type)"
            | kind u /= kind t = fail "kinds do not match"
            | otherwise        = return (u +-> t)


-- | Match the first one to the second.
-- Because the substitution is applied only to one type, this operation is often described as one-way matching
--
-- prop> apply s t1 = t2
match :: Monad m => Type -> Type -> m Subst
match (TAp l r) (TAp l' r')                    = do sl <- match l l'
                                                    sr <- match r r'
                                                    merge sl sr
match (TVar u) t            | kind u == kind t = return (u +-> t)
match (TCon tc1) (TCon tc2) |    tc1 == tc2    = return nullSubst
match _ _                                      = fail "types do not match"
