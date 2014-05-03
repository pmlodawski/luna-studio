---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Package.InstalledPackage where

import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Package.Flag    as Flag
import qualified Flowbox.RepoManager.Data.Package.Package as Package
import           Flowbox.RepoManager.Data.Environment     (Command)
import qualified Flowbox.RepoManager.Data.Types           as Types
import qualified Flowbox.RepoManager.Data.Version         as Version
import qualified Data.ByteString.Lazy.Char8               as BSL
import qualified Data.Digest.Pure.SHA                     as SHA
import qualified Data.List                                as List
import qualified Network.URI                              as URI

data InstalledPackage = InstalledPackage { pkgName      :: Types.QualifiedPackageName
                                         , version      :: Version.Version
                                         , flags        :: [Flag.Flag]
                                         , defaultFlags :: [Flag.Flag]
                                         , enabledFlags :: [Flag.Flag]
                                         , directory    :: FilePath
                                         , dependencies :: [InstalledPackage]
                                         , description  :: String
                                         , source       :: URI.URI
                                         , install      :: [Command]
                                         , uninstall    :: [Command]
                                         , hash         :: SHA.Digest SHA.SHA256State
                                         }

instance Eq InstalledPackage where
    i1 == i2 = hash i1 == hash i2

instance Ord InstalledPackage where
    compare i1 i2 = (pkgName i1 `compare` pkgName i2) ++ (version i1 `compare` version i2)

-- hash is computed as follows:
-- sort flags and take their names
-- sort installed packages and take their hashes
-- concatenate: qualified package name, "-", version, flags, hashes
-- digest resulting string to SHA256
hashPackage :: Package.Package -> [Flag.Flag] -> [InstalledPackage] -> SHA.Digest SHA.SHA256State
hashPackage package explicitFlags deps = SHA.sha256 $ BSL.pack string
    where string = pkgQualifiedName ++ "-" ++ pkgVersion ++ concat sortedFlagsNames ++ concat sortedDepsHashes
          pkgQualifiedName = show $ package ^. Package.pkgName
          pkgVersion       = Version.showVersion $ package ^. Package.version
          sortedFlagsNames = map Flag.name $ List.sort explicitFlags
          sortedDepsHashes = map (SHA.showDigest . hash) $ List.sort deps

makeInstalled :: Package.Package
                 -> FilePath
                 -> [Flag.Flag]
                 -> [InstalledPackage]
                 -> SHA.Digest SHA.SHA256State
                 -> InstalledPackage
makeInstalled package installDir flags' deps hash' = InstalledPackage {
      pkgName      = package ^. Package.pkgName,
      version      = package ^. Package.version,
      flags        = package ^. Package.flags,
      defaultFlags = package ^. Package.defaultFlags,
      enabledFlags = flags',
      directory    = installDir,
      dependencies = deps,
      description  = package ^. Package.description,
      source       = package ^. Package.source,
      install      = package ^. Package.install,
      uninstall    = package ^. Package.uninstall,
      hash         = hash'
    }