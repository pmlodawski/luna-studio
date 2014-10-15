module Luna.Typechecker.Type.Scheme (
    Scheme(..),
    instantiate
  ) where

import Luna.Typechecker.IDs
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Type.Type



data Scheme = Scheme [TyID] Type

instantiate :: (Monad m) => Scheme -> TCLoggerT m Type
instantiate = undefined