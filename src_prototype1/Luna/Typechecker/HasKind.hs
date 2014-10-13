module Luna.Typechecker.HasKind (
    HasKind(..)
  ) where


import Luna.Typechecker.AST.Kind         (Kind(..))
import Luna.Typechecker.AST.Type         (Type(..), Tyvar(..), Tycon(..))

import Luna.Typechecker.Internal.Logger


class HasKind t where
    kind :: (Monad m) => t -> TCLoggerT m Kind


instance HasKind Tyvar where
  kind (Tyvar _ k) = return k

instance HasKind Tycon where
  kind (Tycon _ k) = return k

instance HasKind Type where
  kind (TStruct _ _) = throwError "HasKind.hs:HasKind Type/TStruct not yet defined :(" -- TODO
  kind (TVar u)      = kind u
  kind (TCon tc)     = kind tc
  kind (TAp t t')    = do kt <- kind t
                          kt' <- kind t'
                          case kt of
                            Kfun k' k | kt' == k' -> return k
                            _                     -> throwError "kind mismatch"
  kind (TGen _)      = throwError "HasKind.hs:HasKind Type/TGen should never be asked for kind!"
