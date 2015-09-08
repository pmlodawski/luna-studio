---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.FileManager.Version where

import qualified Data.Version as Version

import qualified Flowbox.FileManager.Config as Config
import           Flowbox.Prelude



full :: Bool -> String
full = fileManager


fileManager :: Bool -> String
fileManager numeric = (if numeric then "" else "Flowbox S3 file manager version ") ++ Version.showVersion Config.version
