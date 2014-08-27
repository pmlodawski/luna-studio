module Luna.Typechecker.Internal.Unification (mgu, match) where

import qualified Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Luna.Typechecker.Internal.HasKind          as HKd
import qualified Luna.Typechecker.Internal.Substitutions    as Sub




-- | Most general unifier.
mgu :: Monad m => Ty.Type -> Ty.Type -> m Sub.Subst
mgu (Ty.TAp l r) (Ty.TAp l' r')              = do s1 <- mgu l l'
                                                  s2 <- mgu (Sub.apply s1 r) (Sub.apply s1 r')
                                                  return (s2 Sub.@@ s1)
mgu (Ty.TVar u) t                            = varBind u t
mgu t (Ty.TVar u)                            = varBind u t
mgu (Ty.TCon tc1) (Ty.TCon tc2) | tc1 == tc2 = return Sub.nullSubst
mgu _ _                                      = fail "types do not unify"


-- | Unify type variable with a type.
-- In practice tests are required to ensure that this is valid, including
-- an occurs check ('u `elem` tv t') and a test to ensure that the substitution is kind-preserving.
varBind :: Monad m => Ty.Tyvar -> Ty.Type -> m Sub.Subst
varBind u t | t == Ty.TVar u           = return Sub.nullSubst
            | u `elem` Sub.tv t        = fail "occurs check fail (can't build infinite type)"
            | HKd.kind u /= HKd.kind t = fail "kinds do not match"
            | otherwise                = return (u Sub.+-> t)


-- | Match the first one to the second.
-- Because the substitution is applied only to one type, this operation is often described as one-way matching
--
-- prop> apply s t1 = t2
match :: Monad m => Ty.Type -> Ty.Type -> m Sub.Subst
match (Ty.TAp l r) (Ty.TAp l' r')                    = do sl <- match l l'
                                                          sr <- match r r'
                                                          Sub.merge sl sr -- TODO [kg]: dlaczego musi być syme
match (Ty.TVar u) t               | HKd.kind u == HKd.kind t = return (u Sub.+-> t)
match (Ty.TCon tc1) (Ty.TCon tc2) |    tc1 == tc2    = return Sub.nullSubst
match _ _                                            = fail "types do not match"
