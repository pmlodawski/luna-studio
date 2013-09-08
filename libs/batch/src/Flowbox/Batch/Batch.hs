---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Batch (
    Batch(..),
    empty,

	attributeKey,
) where

import           Flowbox.Prelude                           
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Batch.Project.ProjectManager   (ProjectManager)



data Batch = Batch { projectManager :: ProjectManager } deriving (Show)


empty :: Batch
empty = Batch ProjectManager.empty


attributeKey :: String
attributeKey = "Batch-0.1"