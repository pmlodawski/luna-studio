---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Control.Monad.Trans.Maybe (
    module Flowbox.Control.Monad.Trans.Maybe,
    module Control.Monad.Trans.Maybe,
    hoistMaybe
) where

import Control.Error.Util        (hoistMaybe)
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Flowbox.Prelude

runMaybe :: MaybeT Identity a -> Maybe a
runMaybe = runIdentity . runMaybeT
