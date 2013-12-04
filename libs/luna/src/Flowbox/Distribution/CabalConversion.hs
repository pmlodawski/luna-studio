---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Distribution.CabalConversion where

import           Data.Default                                    (def)
import           Data.List.Split                                 (splitOn)
import           Data.Monoid
import qualified Distribution.Client.Types                     as CliTypes
import qualified Distribution.InstalledPackageInfo             as Installed
import           Distribution.InstalledPackageInfo               (InstalledPackageInfo)
import qualified Distribution.Package                          as DistPackage
import           Distribution.Package                            (PackageName(PackageName))
import qualified Distribution.PackageDescription               as DistPkgDesc
import           Distribution.PackageDescription                 (PackageDescription)
import qualified Distribution.PackageDescription.Configuration as DistConfig
import qualified Distribution.Version                          as DistVersion

import           Flowbox.Prelude  
import           Flowbox.Data.Version                            (Version(Version))
import qualified Flowbox.Distribution.Package.Package          as Package
import           Flowbox.Distribution.Package.Package            (Package, PackageId(PackageId))


convertSrcPackage :: CliTypes.SourcePackage -> Package
convertSrcPackage pkgInfo = convertPkgDescription desc where
    generic = CliTypes.packageDescription pkgInfo
    desc    = DistConfig.flattenPackageDescription generic


convertPkgDescription :: PackageDescription -> Package
convertPkgDescription desc = def & Package.authors     .~ (splitOn "," $ DistPkgDesc.author desc)
                                 & Package.bugReports  .~ (DistPkgDesc.bugReports desc)
                                 & Package.copyright   .~ (DistPkgDesc.copyright desc)
                                 & Package.description .~ (DistPkgDesc.description desc)
                                 & Package.homepage    .~ (DistPkgDesc.homepage desc)
                                 & Package.maintainers .~ (splitOn "," $ DistPkgDesc.maintainer desc)
                                 & Package.synopsis    .~ (DistPkgDesc.synopsis desc)
                                 & Package.tags        .~ (splitOn "," $ DistPkgDesc.category desc)
                                 & Package.url         .~ (DistPkgDesc.pkgUrl desc)
                                 & Package.id          .~ PackageId name version
                            where PackageName name = DistPackage.packageName desc
                                  version          = convertVersion $ DistPackage.packageVersion desc

convertInstPackage :: InstalledPackageInfo -> Package
convertInstPackage desc = def & Package.authors     .~ (splitOn "," $ Installed.author desc)
                              & Package.bugReports  .~ mempty
                              & Package.copyright   .~ (Installed.copyright desc)
                              & Package.description .~ (Installed.description desc)
                              & Package.homepage    .~ (Installed.homepage desc)
                              & Package.maintainers .~ (splitOn "," $ Installed.maintainer desc)
                              & Package.synopsis    .~ (Installed.synopsis desc)
                              & Package.tags        .~ (splitOn "," $ Installed.category desc)
                              & Package.url         .~ (Installed.pkgUrl desc)
                              & Package.id          .~ PackageId name version
                        where PackageName name = DistPackage.packageName desc
                              version          = convertVersion $ DistPackage.packageVersion desc
   
convertVersion :: DistVersion.Version -> Version
convertVersion dv = Version (DistVersion.versionBranch dv) (DistVersion.versionTags dv)