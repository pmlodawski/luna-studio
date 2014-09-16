module Luna.Typechecker.HasKind (HasKind(..)) where

import           Luna.Typechecker.AST.Kind         (Kind(..))
import           Luna.Typechecker.AST.Type         (Type(..), Tyvar(..), Tycon(..))

class HasKind t where
    kind :: t -> Kind -- ^ Determine the kind of a type variable, type constant, or type expression.

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind (TVar u)   = kind u
  kind (TCon tc)  = kind tc
  kind (TAp t t')  = case kind t of
                         Kfun k' k | kind t' == k' -> k
                         _                         -> error "kind mismatch"
  kind (TGen _)   = error "HasKind.hs:HasKind Type/TGen should never be asked for kind!"

-- TODO [kgdk] 14 sie 2014: zmienić typ zwracany kind na Either by obsługiwać errory
