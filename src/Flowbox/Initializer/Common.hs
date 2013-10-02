---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Initializer.Common where

import qualified Flowbox.System.UniPath as UniPath
import           Flowbox.System.UniPath   (UniPath)



flowboxPath :: UniPath
flowboxPath = UniPath.fromUnixString "~/.flowbox"
