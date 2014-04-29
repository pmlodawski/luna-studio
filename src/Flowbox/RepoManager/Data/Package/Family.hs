---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Package.Family where

import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Package.Flag    as Flag
import qualified Flowbox.RepoManager.Data.Package.Package as Package
import qualified Flowbox.RepoManager.Data.Version         as Version
import qualified Data.Map                                 as Map

data PackageFamily = PackageFamily { name       :: String
                                   , versions   :: Map.Map Version.Version Package.Package
                                   , flagsSet   :: [Flag.Flag]
                                   , flagsUnset :: [Flag.Flag]
                                   }
