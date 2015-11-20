---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Control.Monad.Error (
    module X,
    hoistEither
) where

import Control.Monad.Error as X
import Data.Either

hoistEither :: MonadError a m => Either a b -> m b
hoistEither = either throwError return
