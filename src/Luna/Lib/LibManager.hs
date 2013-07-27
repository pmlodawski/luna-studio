---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Lib.LibManager(
    module Luna.Data.Graph,
    LibManager(..)
) where

import           Luna.Lib.Library       (Library)
import           Luna.Lib.Edge          (Edge)

import           Luna.Data.Graph         hiding(Graph, Edge)
import qualified Luna.Data.Graph         as DG


type LibManager = DG.Graph Library Edge
