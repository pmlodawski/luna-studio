---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Repository where

import Data.Map (Map)

import           Flowbox.Prelude
import           Flowbox.RepoManager.Data.Item.Family.Available (AvailableFamilies)
import           Flowbox.RepoManager.Data.Item.Family.Installed (InstalledFamilies)
import qualified Flowbox.RepoManager.Data.Item.Item             as Item



data Repository = Repository { items :: Map Item.Name AvailableFamilies
                             } deriving (Show)


data World = World { installed :: Map Item.Name InstalledFamilies
                   , selected  :: Map Item.Name InstalledFamilies
                   } deriving (Show)


