---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Batch (
    Batch(..),
    make,

    attributeKey,
) where

import           Flowbox.Prelude                        
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Batch.Project.ProjectManager   (ProjectManager)
import           Flowbox.Config.Config                  (Config)



data Batch = Batch { config         :: Config
                   , projectManager :: ProjectManager
                   } deriving (Show)


make :: Config -> Batch
make config = Batch config ProjectManager.empty


attributeKey :: String
attributeKey = "Batch-0.1"