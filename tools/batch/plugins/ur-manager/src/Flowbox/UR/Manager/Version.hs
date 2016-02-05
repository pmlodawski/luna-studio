---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.UR.Manager.Version where

import qualified Data.Version              as Version

import           Flowbox.Prelude
import qualified Flowbox.UR.Manager.Config as Config



full :: Bool -> String
full = manager


manager :: Bool -> String
manager numeric = (if numeric then "" else "Flowbox undo-redo version ") ++ Version.showVersion Config.version

---- TO POWINNO BYC TAK:
----version = Version.showVersion Config.version

--showVersion numeric = if numeric then ...
--	                             else ...
