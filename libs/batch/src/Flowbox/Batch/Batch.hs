---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Batch (
    Batch(..),
    empty,

) where

import qualified Flowbox.Batch.Project.Project        as Project
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Batch.Project.ProjectManager   (ProjectManager)



data Batch = Batch { projectManager  :: ProjectManager
                   , activeProjectID :: Project.ID
                   } deriving (Show)


empty :: Batch
empty = Batch ProjectManager.empty (-1)
