---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Broker.Version where

import qualified Data.Version as Version

import qualified Flowbox.Broker.Config as Config
import           Flowbox.Prelude



full :: Bool -> String
full = broker


broker :: Bool -> String
broker numeric = (if numeric then "" else "Flowbox broker version ") ++ Version.showVersion Config.version
