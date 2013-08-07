---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.ProjectManager (
    module Luna.Data.Graph,
    ProjectManager
) where

import           Luna.Project           (Project)

import           Luna.Data.Graph         hiding(Graph, Edge)
import qualified Luna.Data.Graph         as DG


type ProjectManager = DG.Graph Project ()
