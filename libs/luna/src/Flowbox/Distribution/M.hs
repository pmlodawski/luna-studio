{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Distribution.M where

import qualified Data.Map                                      as Map

import           Flowbox.Prelude                                 
import qualified Flowbox.Config.Config                         as Config
import qualified Data.Aeson                                    as JSON
import qualified Flowbox.Distribution.Package.PackageFamily    as PackageFamily
import qualified Flowbox.Distribution.Package.PackageIndex     as PackageIndex


main = do
    cfg <- Config.load
    srcPkgIdx  <- PackageIndex.readSrcPkgIdx cfg
    instPkgIdx <- PackageIndex.readInstPkgIdx cfg
    let srcPkgs    = PackageIndex.searchByNameSubstring srcPkgIdx "parsec3"
        instPkgs   = PackageIndex.searchByNameSubstring instPkgIdx "parsec3"
        srcPkgMap  = PackageIndex.partitionByName srcPkgs
        instPkgMap = PackageIndex.partitionByName instPkgs
        pkgMap     = PackageIndex.combinePkgMaps srcPkgMap instPkgMap
        pkgFMap = Map.map PackageFamily.mk pkgMap

    print "hello"
    print $ pkgFMap
    print $ JSON.encode pkgFMap



--readPackages :: IO ([Package], [Package])
--readPackages = do
--    cfg <- Config.load
--    let verbosity   = Verbosity.normal
--        pkgDBs      = localPkgStack cfg
--        globalFlags = defaultGlobalFlags cfg
--    (_, cabalCfg) <- Sandbox.loadConfigOrSandboxConfig verbosity globalFlags mempty
--    let
--        configFlags  = CabalConf.savedConfigureFlags cabalCfg
--        globalFlags' = CabalConf.savedGlobalFlags    cabalCfg `mappend` globalFlags
--        repos        = Setup.globalRepos globalFlags'
--        pats         = ["lhae"]
--    (comp, _, conf) <- configCompilerAux' configFlags
--    --pkgs            <- List.getPkgList verbosity
--    --                                   (configPackageDB' configFlags)
--    --                                   repos
--    --                                   comp
--    --                                   conf
--    --                                   mempty
--    --                                   pats


--    -- Distribution.Client.Types.SourcePackageDb
--    installedPkgIndex <- IndexUtils.getInstalledPackages verbosity comp pkgDBs conf
--    sourcePkgDb       <- IndexUtils.getSourcePackages verbosity repos
--    let matchingPackages search index = [ pkg | pat <- pats, pkg <- search index pat ]

--        sourcePkgIndex :: SourcePackageIndex.PackageIndex CliTypes.SourcePackage
--        sourcePkgIndex = CliTypes.packageIndex sourcePkgDb

--        sourcePkgInfo :: [CliTypes.SourcePackage]
--        sourcePkgInfo  = SourcePackageIndex.allPackages sourcePkgIndex

--        sourceGenerics = fmap CliTypes.packageDescription sourcePkgInfo

--        cSrcPkgs :: [PackageDescription]
--        cSrcPkgs       = fmap DistConfig.flattenPackageDescription sourceGenerics

--        cInstPkgs :: [InstalledPackageInfo]
--        cInstPkgs      = InstalledPackageIndex.allPackages installedPkgIndex

--        srcPkgs        = fmap convertPkgDescription cSrcPkgs
--        instPkgs       = fmap convertInstPackage cInstPkgs

--    return (srcPkgs, instPkgs)






