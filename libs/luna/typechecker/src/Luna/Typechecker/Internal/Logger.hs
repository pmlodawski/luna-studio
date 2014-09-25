module Luna.Typechecker.Internal.Logger (
    module Logger,
    TCLoggerT, MonadError(..)
  ) where


import Control.Monad.Error (MonadError(..))

import Logger


type TCLoggerT = LoggerT String
