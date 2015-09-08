---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Dependency where

import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Package.Flag as Flag
import qualified Flowbox.RepoManager.Data.Types        as Types
import qualified Flowbox.RepoManager.Data.Version      as Version
import qualified System.FilePath                       as FilePath
import qualified Distribution.Version                  as CabalVersion

data Dependency = Dependency { qualDepName   :: Types.QualifiedPackageName
                             , constraints   :: CabalVersion.VersionRange
                             , flagsRequired :: [Flag.Flag]
                             } deriving (Show)