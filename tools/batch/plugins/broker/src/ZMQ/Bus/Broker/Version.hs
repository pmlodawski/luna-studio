---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module ZMQ.Bus.Broker.Version where

import qualified Data.Version          as Version

import           Flowbox.Prelude
import qualified ZMQ.Bus.Broker.Config as Config



full :: Bool -> String
full = broker


broker :: Bool -> String
broker numeric = (if numeric then "" else "Flowbox broker version ") ++ Version.showVersion Config.version
