---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.ProjectManager (
    module Flowbox.Luna.Data.Graph,
    ProjectManager
) where

import           Flowbox.Luna.Project           (Project)

import           Flowbox.Luna.Data.Graph         hiding(Graph, Edge)
import qualified Flowbox.Luna.Data.Graph         as DG


type ProjectManager = DG.Graph Project ()
