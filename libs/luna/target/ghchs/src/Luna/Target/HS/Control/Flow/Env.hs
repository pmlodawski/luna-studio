---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Target.HS.Control.Flow.Env (
    module Luna.Target.HS.Control.Flow.Env,
    module X
) where

import Luna.Target.HS.Control.Context as X
import Luna.Target.HS.Control.Error   as X

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

val = Value . Pure . Safe


