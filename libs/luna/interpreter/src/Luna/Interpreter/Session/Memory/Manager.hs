---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Memory.Manager where

import           Flowbox.Prelude
import           Luna.Interpreter.Session.Env.State     (Session)
import           Luna.Interpreter.Session.Memory.Status (Status)
import qualified Luna.Interpreter.Session.Memory.Status as Status



class MemoryManager mm where
    clean  :: mm -> Status -> Session ()

    cleanIfNeeded :: mm -> Session ()
    cleanIfNeeded mm = do
        status <- Status.status
        when (Status.isUpperLimitExceeded status) $
            clean mm status
