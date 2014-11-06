module Luna.Typechecker.RefactorMePlease (
    mkTyID
  ) where

import Luna.Typechecker.IDs       (TyID(..))
import Luna.Typechecker.TIMonad   (TILogger,getNextID)
import Luna.Typechecker.Type.Type (Type(..),Tyvar(..))

import Control.Applicative
import Control.Monad.Trans



mkTyID :: TILogger Type
mkTyID = (TVar . Tyvar . TyID . show) <$> lift getNextID