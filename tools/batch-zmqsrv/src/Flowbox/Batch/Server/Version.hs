---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.Version where

import qualified Data.Version                as Version

import           Flowbox.Prelude               
import qualified Flowbox.Batch.Config        as BatchConfig
import qualified Flowbox.Batch.Server.Config as Config
import qualified Flowbox.Luna.Config.Config  as LibConfig



full :: Bool -> String
full numeric = server numeric ++ "\n" ++ batch numeric ++ "\n" ++ library numeric


server :: Bool -> String
server numeric = (if numeric then "" else "Batch ZMQ server version ") ++ Version.showVersion Config.version


batch :: Bool -> String
batch numeric = (if numeric then "" else "Batch library version ") ++ Version.showVersion BatchConfig.version


library :: Bool -> String
library numeric = (if numeric then "" else "Luna library version ") ++ Version.showVersion LibConfig.version
