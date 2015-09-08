---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.InstanceManager.Version where

import qualified Data.Version as Version

import qualified Flowbox.InstanceManager.Config as Config
import           Flowbox.Prelude



full :: Bool -> String
full = nimbus


nimbus :: Bool -> String
nimbus numeric = (if numeric then "" else "Flowbox Instance Manager version ") ++ Version.showVersion Config.version
