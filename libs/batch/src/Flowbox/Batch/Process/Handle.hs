---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Process.Handle where

import Flowbox.Prelude
import Flowbox.System.Process (ProcessHandle)



data Handle = Handle { processHandle :: ProcessHandle
                     }


instance Show Handle where
    show _ = "Handle {...}"
