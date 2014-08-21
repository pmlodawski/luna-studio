module Flowbox.Luna.Typechecker.Internal.HasKind (HasKind(..)) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty




-- TODO [kgdk] 21 sie 2014: zmienić nazwę HaskKind -> ClassHasKind

class HasKind t where
    kind :: t -> Knd.Kind -- ^ Determine the kind of a type variable, type constant, or type expression.

instance HasKind Ty.Tyvar where
  kind (Ty.Tyvar _ k) = k

instance HasKind Ty.Tycon where
  kind (Ty.Tycon _ k) = k

instance HasKind Ty.Type where
  kind (Ty.TVar u)   = kind u
  kind (Ty.TCon tc)  = kind tc
  kind (Ty.TAp t _)  = case kind t of
                         (Knd.Kfun _ k) -> k
                         _              -> error "kind mismatch"
                         -- TODO [kgdk] 21 sie 2014: checking, że t' == k'
  kind (Ty.TGen _)   = error "HasKind.hs:HasKind Ty.Type/TGen should never be asked for kind!"

-- TODO [kgdk] 14 sie 2014: zmienić typ zwracany kind na Either by obsługiwać errory
