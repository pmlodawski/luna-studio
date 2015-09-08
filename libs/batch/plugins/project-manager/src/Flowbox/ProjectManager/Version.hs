---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.ProjectManager.Version where

import qualified Data.Version as Version

import           Flowbox.Prelude
import qualified Flowbox.ProjectManager.Config as Config



full :: Bool -> String
full = projectManager


projectManager :: Bool -> String
projectManager numeric = (if numeric then "" else "Flowbox project manager version ") ++ Version.showVersion Config.version
