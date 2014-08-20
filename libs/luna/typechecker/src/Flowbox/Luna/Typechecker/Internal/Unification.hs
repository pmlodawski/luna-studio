module Flowbox.Luna.Typechecker.Internal.Unification (mgu, match) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.AST    as AST
import qualified Flowbox.Luna.Typechecker.Internal.AST.Common as Com
import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr   as Exp
import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind   as Knd
import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit    as Lit
import qualified Flowbox.Luna.Typechecker.Internal.AST.Module as Mod
import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat    as Pat
import qualified Flowbox.Luna.Typechecker.Internal.AST.TID    as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type   as Ty

import           Flowbox.Luna.Data.AST.Common                 (ID(..))
import           Flowbox.Luna.Typechecker.Internal.AST.TID    (TID(..))



-- | Most general unifier.
-- Unification is obviously a partial function.
mgu :: Monad m => Ty.Type -> Ty.Type -> m Subst
mgu (Ty.TAp l r) (Ty.TAp l' r')              = do s1 <- mgu l l'
                                                  s2 <- mgu (apply s1 r) (apply s1 r')
                                                  return (s2 @@ s1)
mgu (Ty.TVar u) t                            = varBind u t
mgu t (Ty.TVar u)                            = varBind u t
mgu (Ty.TCon tc1) (Ty.TCon tc2) | tc1 == tc2 = return nullSubst
mgu t1 t2                                    = fail "types do not unify"


-- TODO [kgdk] 14 sie 2014: nie lubimy się z 'fail' za bardzo. Fix here and there
-- TODO [kgdk] 18 sie 2014: naprawić poprzez… co? class Monad m => MyError m where err :: a -> m a ?
-- czy jest jakiś odpowiednik


-- | Unify type variable with a type.
-- In practice tests are required to ensure that this is valid, including
-- an occurs check ('u `elem` tv t') and a test to ensure that the substitution is kind-preserving.
varBind :: Monad m => Ty.Tyvar -> Ty.Type -> m Subst
varBind u t | t == Ty.TVar u   = return nullSubst
            | u `elem` tv t    = fail "occurs check fail (can't build infinite type)"
            | kind u /= kind t = fail "kinds do not match"
            | otherwise        = return (u +-> t)


-- | Match the first one to the second.
-- Because the substitution is applied only to one type, this operation is often described as one-way matching
--
-- prop> apply s t1 = t2
match :: Monad m => Ty.Type -> Ty.Type -> m Subst
match (Ty.TAp l r) (Ty.TAp l' r')                    = do sl <- match l l'
                                                          sr <- match r r'
                                                          merge sl sr -- TODO [kg]: dlaczego musi być syme
match (Ty.TVar u) t               | kind u == kind t = return (u +-> t)
match (Ty.TCon tc1) (Ty.TCon tc2) |    tc1 == tc2    = return nullSubst
match t1 t2                                          = fail "types do not match"