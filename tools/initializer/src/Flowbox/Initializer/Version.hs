---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Initializer.Version where

import qualified Data.Version as Version

import qualified Flowbox.Initializer.Config as Config
import           Flowbox.Prelude



full :: Bool -> String
full = initializer


initializer :: Bool -> String
initializer numeric = (if numeric then "" else "Luna initializer version ") ++ Version.showVersion Config.version
