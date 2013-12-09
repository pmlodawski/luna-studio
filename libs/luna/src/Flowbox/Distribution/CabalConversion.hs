---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Distribution.CabalConversion where

import           Data.Default                                  (def)
import           Data.List.Split                               (splitOn)
import           Data.Monoid
import qualified Distribution.Client.Types                     as CliTypes
import           Distribution.InstalledPackageInfo             (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo             as Installed
import qualified Distribution.License                          as DistLicense
import           Distribution.Package                          (PackageName (PackageName))
import qualified Distribution.Package                          as DistPackage
import           Distribution.PackageDescription               (PackageDescription)
import qualified Distribution.PackageDescription               as DistPkgDesc
import qualified Distribution.PackageDescription.Configuration as DistConfig
import qualified Distribution.Version                          as DistVersion

import           Flowbox.Data.Version                 (Version (Version))
import           Flowbox.Distribution.License         (License)
import qualified Flowbox.Distribution.License         as License
import           Flowbox.Distribution.Package.Package (Package, PackageId (PackageId))
import qualified Flowbox.Distribution.Package.Package as Package
import           Flowbox.Prelude


convertSrcPackage :: CliTypes.SourcePackage -> Package
convertSrcPackage pkgInfo = convertPkgDescription desc name where
    name    = DistPackage.packageName pkgInfo
    generic = CliTypes.packageDescription pkgInfo
    --desc    = DistPkgDesc.packageDescription generic
    desc    = DistConfig.flattenPackageDescription generic


convertPkgDescription :: PackageDescription -> PackageName -> Package
convertPkgDescription desc (PackageName name) = def { Package._id = PackageId name version }
                                  & Package.authors     .~ (splitOn "," $ DistPkgDesc.author desc)
                                  & Package.bugReports  .~ (DistPkgDesc.bugReports desc)
                                  & Package.license     .~ (convertLicense $ DistPkgDesc.license desc)
                                  & Package.copyright   .~ (DistPkgDesc.copyright desc)
                                  & Package.description .~ (DistPkgDesc.description desc)
                                  & Package.homepage    .~ (DistPkgDesc.homepage desc)
                                  & Package.maintainers .~ (splitOn "," $ DistPkgDesc.maintainer desc)
                                  & Package.synopsis    .~ (DistPkgDesc.synopsis desc)
                                  & Package.tags        .~ (splitOn "," $ DistPkgDesc.category desc)
                                  & Package.url         .~ (DistPkgDesc.pkgUrl desc)
                                  -- & Package.id          .~ PackageId name version
                            where
                                  --PackageName name = DistPackage.packageName desc
                                  --PackageName name' = (DistPackage.pkgName $ DistPkgDesc.package desc)
                                  version          = convertVersion $ DistPackage.packageVersion desc

convertInstPackage :: InstalledPackageInfo -> Package
convertInstPackage desc = def & Package.authors     .~ (splitOn "," $ Installed.author desc)
                              & Package.bugReports  .~ mempty
                              & Package.license     .~ (convertLicense $ Installed.license desc)
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


convertLicense :: DistLicense.License -> License
convertLicense lic = case lic of
    DistLicense.GPL  mversion     -> License.GPL  (fmap convertVersion mversion)
    DistLicense.LGPL mversion     -> License.LGPL (fmap convertVersion mversion)
    DistLicense.BSD3              -> License.BSD3
    DistLicense.MIT               -> License.MIT
    DistLicense.PublicDomain      -> License.PublicDomain
    DistLicense.AllRightsReserved -> License.AllRightsReserved
    DistLicense.OtherLicense      -> License.UnknownLicense
    DistLicense.UnknownLicense s  -> License.OtherLicense s
    other                         -> License.OtherLicense $ show other

