module Luna.Typechecker.Internal.Substitutions (Subst, Types(..), (@@), merge, nullSubst, (+->)) where

import qualified Luna.Typechecker.Internal.AST.Type         as Ty



import           Data.List                                          (intersect,nub,union)
import           Data.Maybe                                         (fromMaybe)

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




-- --------------------------------------------------------------------------------

-- TODO [kgdk] 21 sie 2014: przenieść to co poniżej gdzieś (było: recursive import)
class Types t where
  apply :: Subst -> t -> t -- ^ Substitutions can be applied to types-and, in fact, to any other value with type components-in a natural way.
  tv :: t -> [Ty.Tyvar]    -- ^ Returns the set of type variables (i.e., Tyvars) appearing in its argument, listed in order of first occurrence (from left to right), with no duplicates.


instance Types Ty.Type where
  apply s (Ty.TVar u)  = fromMaybe (Ty.TVar u) (lookup u s)
  apply s (Ty.TAp l r) = Ty.TAp (apply s l) (apply s r)
  apply _ t            = t -- no substitution for TGen and TCon
  tv (Ty.TVar u)  = [u]
  tv (Ty.TAp l r) = tv l `union` tv r
  tv _            = [] -- no type variables from TGen and TCon

-- TODO [kgdk] 14 sie 2014: czy apply/tv dla TGen/TCon powinno rzucać error czy działać?


instance Types a => Types [a] where
  apply s = map (apply s)
  tv = nub . concatMap tv -- O(n^2)

-- TODO [kgdk] 14 sie 2014: poprawić implementację 'tv', by była w czasie O(n log n). Na 95% konieczne będzie
-- zachowanie kolejności elementów.

