module Luna.Typechecker.HasKind (
    HasKind(..)
  ) where

import           Luna.Typechecker.AST.Kind         (Kind(..))
import           Luna.Typechecker.AST.Type         (Type(..), Tyvar(..), Tycon(..))

class HasKind t where
    kind :: t -> Kind

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
