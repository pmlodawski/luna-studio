---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Lib.LibManager(
    module Flowbox.Luna.Data.Graph,
    LibManager,
    empty
) where

import           Flowbox.Luna.Lib.Library       (Library)
import           Flowbox.Luna.Lib.Edge          (Edge)

import           Flowbox.Luna.Data.Graph         hiding(Graph, Edge, empty)
import qualified Flowbox.Luna.Data.Graph         as DG


type LibManager = DG.Graph Library Edge

empty :: LibManager
empty = DG.empty
