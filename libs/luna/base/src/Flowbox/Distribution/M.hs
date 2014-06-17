{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Flowbox.Distribution.M where

--import qualified Data.Map as Map

--import qualified Data.Aeson  as JSON
--import           Data.String (fromString)

--import qualified Flowbox.Config.Config                        as Config
--import qualified Flowbox.Data.Version                         as Version
--import           Flowbox.Distribution.Package.PackageFamily   (PackageFamily)
--import qualified Flowbox.Distribution.Package.PackageFamily   as PackageFamily
--import qualified Flowbox.Distribution.Package.PackageIndex    as PackageIndex
--import           Flowbox.Prelude
--import           Flowbox.System.Console.StyledText.StyledText (StyledText)
--import qualified Flowbox.System.Console.StyledText.StyledText as StyledText

--import Data.String.Utils (join)

--import           Data.List                                     (groupBy, sortBy)
--import           Debug.Trace
--import qualified Distribution.Client.Types                     as CliTypes
--import qualified Distribution.InstalledPackageInfo             as Installed
--import           Distribution.Package                          (PackageName, packageName)
--import qualified Distribution.Package                          as DistPackage
--import qualified Distribution.PackageDescription               as DistPkgDesc
--import qualified Distribution.PackageDescription.Configuration as DistConfig
--import           Distribution.Simple.Utils                     (comparing, die, equating, notice)
--import qualified Flowbox.Distribution.CabalConversion          as CabalConversion
--import           Flowbox.Distribution.Package.Package          (Package)
--import qualified Flowbox.Distribution.Package.Package          as Package

--import qualified Data.Set as Set

--main = do
--    cfg <- Config.load
--    srcPkgIdx  <- PackageIndex.readSrcPkgIdx cfg
--    instPkgIdx <- PackageIndex.readInstPkgIdx cfg
--    let srcPkgs    = PackageIndex.searchByNameSubstring srcPkgIdx "x"
--        instPkgs   = PackageIndex.searchByNameSubstring instPkgIdx "x"
--        srcPkgMap  = PackageIndex.partitionByName srcPkgs
--        instPkgMap = PackageIndex.partitionByName instPkgs
--        pkgMap     = PackageIndex.combinePkgMaps srcPkgMap instPkgMap
--        pkgFMap = Map.map PackageFamily.mk pkgMap

--    print "hello"
--    print $ map (view $ Package.id . Package.name) instPkgs
--    mapM_ PackageFamily.print $ Map.elems pkgFMap

-----------------------------------------------------

--showPF :: PackageFamily -> String
--showPF pf =


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






