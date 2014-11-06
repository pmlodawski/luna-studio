---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Lib.Manager (
    module Flowbox.Data.Graph,
    module Luna.Lib.Manager,
) where

import Flowbox.Data.Graph hiding (Edge)
import Luna.Lib.Edge      (Edge)
import Luna.Lib.Lib       (Library)



type LibManager = Graph Library Edge

