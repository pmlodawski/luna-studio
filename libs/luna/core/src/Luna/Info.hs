---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Info (
    module Luna.Info,
    module Paths_luna_core
) where

import Flowbox.Prelude
import Paths_luna_core (version)
import Flowbox.Data.Version

apiVersion :: Versioned String
apiVersion = Versioned "Luna" $ Version [0,1] []