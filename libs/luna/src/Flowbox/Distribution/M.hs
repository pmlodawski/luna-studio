{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Distribution.M where

import           Flowbox.Prelude                                 
import qualified Distribution.Client.Setup                     as Setup
import           Distribution.Client.Setup                       (GlobalFlags)
import           Data.Monoid                                     
import           Distribution.Simple.Compiler                    (PackageDB(GlobalPackageDB, SpecificPackageDB))
import qualified Flowbox.Config.Config                         as Config
import           Flowbox.Config.Config                           (Config)
import           Distribution.Verbosity                        as Verbosity
import           Distribution.Client.Sandbox                   as Sandbox
import           Distribution.Simple.Setup                       (Flag(Flag))
--import qualified Distribution.Client.List             as List
import qualified Distribution.Client.Config                    as CabalConf

import qualified Distribution.ModuleName                       as ModuleName
import qualified Distribution.Client.IndexUtils                as IndexUtils

import qualified Flowbox.Distribution.Package.Package          as Package
import qualified Flowbox.Distribution.Package.PackageFamily    as PackageFamily
import           Flowbox.Distribution.Package.Package            (Package(Package), PackageId(PackageId))

import qualified Distribution.Package                          as DistPackage
import           Distribution.Package                            (PackageName(PackageName))

import qualified Distribution.InstalledPackageInfo             as Installed
import           Distribution.InstalledPackageInfo               (InstalledPackageInfo)
import qualified Distribution.Client.Types                     as CliTypes
import qualified Distribution.Client.PackageIndex              as SourcePackageIndex
import qualified Distribution.Simple.PackageIndex              as InstalledPackageIndex

import qualified Distribution.PackageDescription               as DistPkgDesc
import           Distribution.PackageDescription                 (GenericPackageDescription, PackageDescription)
import qualified Distribution.PackageDescription.Configuration as DistConfig

import           Control.Applicative                             

import           Data.Aeson                                      
import           Data.Aeson.TH                                   
import           Data.Char                                       (toLower)

import           GHC.Generics                                    
import           Data.Default                                    (Default, def)
import           Data.Maybe                                      ( listToMaybe, fromJust, fromMaybe, isJust )
import qualified Distribution.Version                          as DistVersion


import qualified Flowbox.Data.Version                          as Version
import           Flowbox.Data.Version                            (Version(Version))
import qualified Data.Map                                      as Map
import           Data.Map                                        (Map)
import qualified Data.List                                     as List
import           Data.Char                                       (toLower)
import           Data.List.Split                                 (splitOn)


data PackageIndex = InstalledPackageIndex InstalledPackageIndex.PackageIndex
                  | SourcePackageIndex   (SourcePackageIndex.PackageIndex CliTypes.SourcePackage)
                  deriving (Show)

localPkgDB :: Config -> PackageDB
localPkgDB = SpecificPackageDB . Config.pkgDb . Config.local

globalPkgDB :: Config -> PackageDB
globalPkgDB = SpecificPackageDB . Config.pkgDb . Config.global

localPkgStack :: Config -> [PackageDB]
localPkgStack cfg = [ GlobalPackageDB
                    , localPkgDB  cfg
                    , globalPkgDB cfg
                    ]

globalPkgStack :: Config -> [PackageDB]
globalPkgStack cfg = [ GlobalPackageDB
                     , globalPkgDB cfg
                     , localPkgDB  cfg
                     ]

defaultGlobalFlags :: Config -> GlobalFlags
defaultGlobalFlags cfg = mempty { Setup.globalConfigFile = Flag $ (Config.cabal . Config.config) cfg }


readCabalCfg :: Config -> IO CabalConf.SavedConfig
readCabalCfg cfg = do
    let globalFlags = defaultGlobalFlags cfg
    (_, cabalCfg) <- Sandbox.loadConfigOrSandboxConfig Verbosity.normal globalFlags mempty
    return cabalCfg


readSrcPkgIdx :: Config -> IO PackageIndex
readSrcPkgIdx cfg = do
    cabalCfg <- readCabalCfg cfg
    let globalFlags  = defaultGlobalFlags cfg
        globalFlags' = CabalConf.savedGlobalFlags cabalCfg `mappend` globalFlags
        repos        = Setup.globalRepos globalFlags'
    sourcePkgDb <- IndexUtils.getSourcePackages Verbosity.normal repos
    return $ SourcePackageIndex (CliTypes.packageIndex sourcePkgDb)


readInstPkgIdx :: Config -> IO PackageIndex
readInstPkgIdx cfg = do
    cabalCfg <- readCabalCfg cfg
    let pkgDBs      = localPkgStack cfg
        configFlags = CabalConf.savedConfigureFlags cabalCfg
    (comp, _, conf)   <- configCompilerAux' configFlags
    installedPkgIndex <- IndexUtils.getInstalledPackages Verbosity.normal comp pkgDBs conf
    return $ InstalledPackageIndex installedPkgIndex


searchByNameSubstring :: PackageIndex -> String -> [Package]
searchByNameSubstring idx pattern = case idx of
    InstalledPackageIndex pidx -> map convertInstPackage $ InstalledPackageIndex.searchByNameSubstring pidx pattern
    SourcePackageIndex    pidx -> map convertSrcPackage  $ concatMap snd (SourcePackageIndex.searchByNameSubstring pidx pattern)



readPackages :: IO ([Package], [Package])
readPackages = do
    cfg <- Config.load
    let verbosity   = Verbosity.normal
        pkgDBs      = localPkgStack cfg
        globalFlags = defaultGlobalFlags cfg
    (_, cabalCfg) <- Sandbox.loadConfigOrSandboxConfig verbosity globalFlags mempty
    let
        configFlags  = CabalConf.savedConfigureFlags cabalCfg
        globalFlags' = CabalConf.savedGlobalFlags    cabalCfg `mappend` globalFlags
        repos        = Setup.globalRepos globalFlags'
        pats         = ["lhae"]
    (comp, _, conf) <- configCompilerAux' configFlags
    --pkgs            <- List.getPkgList verbosity
    --                                   (configPackageDB' configFlags)
    --                                   repos
    --                                   comp
    --                                   conf
    --                                   mempty
    --                                   pats


    -- Distribution.Client.Types.SourcePackageDb
    installedPkgIndex <- IndexUtils.getInstalledPackages verbosity comp pkgDBs conf
    sourcePkgDb       <- IndexUtils.getSourcePackages verbosity repos
    let matchingPackages search index = [ pkg | pat <- pats, pkg <- search index pat ]

        sourcePkgIndex :: SourcePackageIndex.PackageIndex CliTypes.SourcePackage
        sourcePkgIndex = CliTypes.packageIndex sourcePkgDb

        sourcePkgInfo :: [CliTypes.SourcePackage]
        sourcePkgInfo  = SourcePackageIndex.allPackages sourcePkgIndex

        sourceGenerics = fmap CliTypes.packageDescription sourcePkgInfo

        cSrcPkgs :: [PackageDescription]
        cSrcPkgs       = fmap DistConfig.flattenPackageDescription sourceGenerics

        cInstPkgs :: [InstalledPackageInfo]
        cInstPkgs      = InstalledPackageIndex.allPackages installedPkgIndex

        srcPkgs        = fmap convertPkgDescription cSrcPkgs
        instPkgs       = fmap convertInstPackage cInstPkgs

    return (srcPkgs, instPkgs)


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


searchByName :: String -> [Package] -> [Package]
searchByName name pkgs = filter (\p -> isSubstr False name $ view (Package.id . Package.name) p ) pkgs


isSubstr :: Bool -> String -> String -> Bool
isSubstr caseSensitive pattern str = List.isInfixOf (f pattern) (f str)
    where f = if caseSensitive then id else map toLower


partitionByName :: [Package] -> Map String [Package]
partitionByName pkgs = foldr insertPkg mempty pkgs
    where insertPkg pkg map = let name = pkg ^. Package.id ^. Package.name
                                  pkgList = pkg : Map.findWithDefault [] name map
                              in  Map.insert name pkgList map



main = do
    cfg <- Config.load
    pkgidx <- readSrcPkgIdx cfg
    let pkgs    = searchByNameSubstring pkgidx "parsec3"
        pkgMap  = partitionByName pkgs
        pkgFMap = Map.map PackageFamily.mk pkgMap
    --(srcPkgs, instPkgs) <- readPackages
    --let out = searchByName "HPong" srcPkgs
    --    names = map (view (Package.id . Package.name)) srcPkgs
    --print names
    print "hello"
    print $ pkgFMap