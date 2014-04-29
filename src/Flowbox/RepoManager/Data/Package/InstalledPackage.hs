---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Package.InstalledPackage where

import           Flowbox.Prelude
import           Flowbox.RepoManager.Data.Package.Flag
import           Flowbox.RepoManager.Data.Environment   (Command)
import qualified Flowbox.RepoManager.Data.Version       as Version
import qualified Network.URI as URI

type SHA256 = String

data InstalledPackage = InstalledPackage { name         :: String
                                         , category     :: [String]
                                         , version      :: Version.Version
                                         , flags        :: [Flag]
                                         , defaultFlags :: [Flag]
                                         , enabledFlags :: [Flag]
                                         , directory    :: FilePath
                                         , dependencies :: [InstalledPackage]
                                         , description  :: String
                                         , source       :: URI.URI
                                         , install      :: [Command]
                                         , uninstall    :: [Command]
                                         , hash         :: SHA256
                                         }
