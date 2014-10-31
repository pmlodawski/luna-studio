module Luna.Typechecker.RefactorMePlease (
    mkTyID
  ) where

import Luna.Typechecker.IDs
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Type.Type

import Control.Applicative
import Control.Monad.Trans


mkTyID :: TILogger Type
mkTyID = mkTyIdWithConstraints noConstraints

mkTyIdWithConstraints :: Constraints -> TILogger Type
mkTyIdWithConstraints constraints = (TVar . Tyvar . TyID . show) <$> lift getNextID where
    tyvar varId = Tyvar varId constraints
