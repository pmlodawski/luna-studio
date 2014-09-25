module Luna.Typechecker.Internal.Logger (
    module Logger,
    TCLoggerT, MonadError(..)
    --Identity, runIdentity
  ) where


import Control.Monad.Error   (MonadError(..))

--import Data.Functor.Identity (Identity,runIdentity)

import Logger


--type TCLogger = LoggerT String Identity
type TCLoggerT = LoggerT String
