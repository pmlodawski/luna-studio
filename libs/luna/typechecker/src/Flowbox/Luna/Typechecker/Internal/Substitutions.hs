module Flowbox.Luna.Typechecker.Internal.Substitutions (
    Subst(..), nullSubst, (+->), (@@), merge
  ) where

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

-- | Type substitution: substitute one type for another.
-- Example: substitute 
type Subst = [(Ty.Tyvar, Ty.Type)]

-- TODO [kgdk] 14 sie 2014: ogarnąć aliasy typów tak, by np. wyświetlać 'String' a nie '[Char]'

-- | Empty substitution.
nullSubst :: Subst
nullSubst = []

-- | Singleton substitution.
(+->) :: Ty.Tyvar -> Ty.Type -> Subst
u +-> t = [(u, t)]

-- TODO [kgdk] 14 sie 2014: wszystko, co wykorzystuje (+->), otestować dokładnie:
-- sprawdzić właściwość, że konstuowane jest 'u +-> t' wyłącznie gdy 'kind u = kind t'

-- |Composition of substitutions. Order matters.
--
-- prop> apply (s1 @@ s2) = apply s1 . apply s2
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1



-- | A `symmetric' composition of substitutions.
-- Concatenation 's1++s2' of two substitutions s1 and s2 works, but the result is left-biased
-- because bindings in s1 take precedence over any bindings for the same variables in s2.
-- Checks if substitutions agree at every variable in the domain of both.
--
-- Ensures that:
-- 
-- prop> apply (s1++s2) = apply (s2++s1)
merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1 ++ s2) else fail "merge fails"
  where agree = all (\v -> apply s1 (Ty.TVar v) == apply s2 (Ty.TVar v))
                    (map fst s1 `intersect` map fst s2)

-- TODO [kgdk] 14 sie 2014: merge działa w czasie O(n^2) przez 'intersect'. Jeśli Subst będzie inaczej
-- reprezentowany to można nawet do O(n) zejść.