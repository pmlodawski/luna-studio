{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Flowbox.Distribution.M where

import qualified Data.Map as Map

import qualified Data.Aeson  as JSON
import           Data.String (fromString)

import qualified Flowbox.Config.Config                        as Config
import qualified Flowbox.Data.Version                         as Version
import           Flowbox.Distribution.Package.PackageFamily   (PackageFamily)
import qualified Flowbox.Distribution.Package.PackageFamily   as PackageFamily
import qualified Flowbox.Distribution.Package.PackageIndex    as PackageIndex
import           Flowbox.Prelude
import           Flowbox.System.Console.StyledText.StyledText (StyledText)
import qualified Flowbox.System.Console.StyledText.StyledText as StyledText

import           Data.String.Utils (join)

import Debug.Trace
import           Flowbox.Distribution.Package.Package (Package)
import qualified Flowbox.Distribution.Package.Package as Package
import           Data.List         (groupBy, sortBy)
import           Distribution.Simple.Utils        (comparing, die, equating, notice)
import qualified Distribution.InstalledPackageInfo             as Installed
import qualified Distribution.Client.Types as CliTypes
import           Distribution.Package (PackageName, packageName)
import qualified Distribution.PackageDescription.Configuration as DistConfig
import qualified Flowbox.Distribution.CabalConversion as CabalConversion
import qualified Distribution.PackageDescription               as DistPkgDesc
import qualified Distribution.Package                          as DistPackage

main = do
    cfg <- Config.load
    srcPkgIdx  <- PackageIndex.readSrcPkgIdx cfg
    instPkgIdx <- PackageIndex.readInstPkgIdx cfg
    let srcPkgs    = PackageIndex.searchByNameSubstring srcPkgIdx ""
        instPkgs   = PackageIndex.searchByNameSubstring instPkgIdx ""
        srcPkgMap  = PackageIndex.partitionByName srcPkgs
        instPkgMap = PackageIndex.partitionByName instPkgs
        pkgMap     = PackageIndex.combinePkgMaps srcPkgMap instPkgMap
        pkgFMap = Map.map PackageFamily.mk pkgMap

    print "hello"
    print $ map (view $ Package.id . Package.name) instPkgs
    mapM_ printPackageFamily $ Map.elems pkgFMap


viewPF :: PackageFamily -> StyledText
viewPF pf = indicator ++ " " ++ title
            ++ showSection "Synopsis:           " (fromString $ pf ^. PackageFamily.synopsis)
            ++ showSection "Homepage:           " (fromString $ pf ^. PackageFamily.homepage)
            ++ showSection "License:            " (fromString $ show $ pf ^. PackageFamily.license)
            ++ showSection "Available versions: " (showVersions $ pf ^. PackageFamily.availableVersions)
            ++ showSection "Installed versions: " (showVersions $ pf ^. PackageFamily.installedVersions)

            where nl = "\n"
                  indent      = fromString $ replicate 4 ' '
                  titleIndent = fromString $ replicate 20 ' '
                  nli         = nl ++ indent
                  isInstalled = not $ null $ pf ^. PackageFamily.installedVersions
                  indicator   = if isInstalled then "[" ++ StyledText.green "I" ++ "]"
                                               else StyledText.green "*"
                  title       = cmod (fromString $ pf ^. PackageFamily.name)
                                where cmod = if isInstalled then StyledText.green else id
                  showSection name content  = if null content then ""
                                              else nli ++ StyledText.blue name ++ content
                  collectVersions vs        = Map.foldrWithKey showVersion [] $ Version.partition 2 vs
                  showVersions vs           = join (nl ++ indent ++ titleIndent) $ collectVersions vs
                  showVersion branch vs lst =  (StyledText.yellow (fromString $ "(" ++ Version.readableBranch branch ++ ") ")
                                            ++ fromString (join ", " (map Version.readable vs))) : lst

printPackageFamily :: PackageFamily -> IO ()
printPackageFamily pf =  (StyledText.print $ viewPF pf)
                      *> putStrLn ""

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






