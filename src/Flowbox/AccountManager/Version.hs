---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.Version where

import qualified Data.Version as Version

import qualified Flowbox.AccountManager.Config as Config
import           Flowbox.Prelude



full :: Bool -> String
full = accountManager


accountManager :: Bool -> String
accountManager numeric = (if numeric then "" else "Flowbox account manager version ") ++ Version.showVersion Config.version
