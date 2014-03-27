---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Item.Item where

import           Data.Map                             (Map)
import           Flowbox.Prelude
import           Flowbox.RepoManager.Data.Dependency  (Dependency)
import           Flowbox.RepoManager.Data.Environment (Architecture, URI, Command)
import qualified Flowbox.RepoManager.Data.Item.Name   as Item
import           Flowbox.RepoManager.Data.Version     (Version)



data Item = Item { name            :: Item.Name
                 , version         :: Version
                 , source          :: Map Architecture URI
                 , installScript   :: [Command]
                 , uninstallScript :: [Command]
                 , dependencies    :: [Dependency]
                 } deriving (Show)

