{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Distribution.M where

import           Flowbox.Prelude                        
import qualified Distribution.Client.Setup            as Setup
import           Distribution.Client.Setup              (GlobalFlags)
import           Data.Monoid                            
import           Distribution.Simple.Compiler           (PackageDB(GlobalPackageDB, SpecificPackageDB))
import qualified Flowbox.Config.Config                as Config
import           Flowbox.Config.Config                  (Config)
import           Distribution.Verbosity               as Verbosity
import           Distribution.Client.Sandbox          as Sandbox
import           Distribution.Simple.Setup              (Flag(Flag))
import qualified Distribution.Client.List             as List
import qualified Distribution.Client.Config           as CabalConf

import qualified Distribution.ModuleName              as ModuleName
import qualified Distribution.Client.IndexUtils       as IndexUtils

import qualified Flowbox.Distribution.Package.Package as Package
import           Flowbox.Distribution.Package.Package   (Package(Package))

import qualified Distribution.Package                 as DistPackage

import qualified Distribution.Client.Types            as CliTypes
import qualified Distribution.Client.PackageIndex     as PackageIndex

import qualified Distribution.PackageDescription      as DistPkgDesc
import           Distribution.PackageDescription        (GenericPackageDescription, PackageDescription)
import qualified Distribution.PackageDescription.Configuration as DistConfig

import           Data.Aeson                             
import           Data.Aeson.TH                          
import           Data.Char                              (toLower)

import           GHC.Generics                           
import           Data.Default                           (Default, def)
import           Data.Maybe                             ( listToMaybe, fromJust, fromMaybe, isJust )
import qualified Distribution.Version                as DistVersion


import qualified Flowbox.Data.Version                as Version
import qualified Data.Map                            as Map
import           Data.List.Split                        (splitOn)


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

main :: IO ()
main = do
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
    pkgs            <- List.getPkgList verbosity
                                       (configPackageDB' configFlags)
                                       repos
                                       comp
                                       conf
                                       mempty
                                       pats

    return ()

    -- Distribution.Client.Types.SourcePackageDb
    sourcePkgDb       <- IndexUtils.getSourcePackages verbosity repos
    let matchingPackages search index = [ pkg | pat <- pats, pkg <- search index pat ]

        sourcePkgIndex :: PackageIndex.PackageIndex CliTypes.SourcePackage
        sourcePkgIndex = CliTypes.packageIndex sourcePkgDb
        -- sourcePkgs ?
        sourcePkgs     = matchingPackages (\ idx n -> concatMap snd (PackageIndex.searchByNameSubstring idx n)) sourcePkgIndex
        
        prefs name     = fromMaybe DistVersion.anyVersion (Map.lookup name (CliTypes.packagePreferences sourcePkgDb))
        pkgname        = DistPackage.PackageName "lhae" -- fix me
        pref           = prefs pkgname

        selectedPkg :: Maybe CliTypes.SourcePackage
        selectedPkg    = List.latestWithPref pref sourcePkgs
        
        sourceSelected = selectedPkg

        sourceGeneric :: Maybe GenericPackageDescription
        sourceGeneric  = fmap CliTypes.packageDescription sourceSelected

        source :: Maybe PackageDescription
        source         = fmap DistConfig.flattenPackageDescription sourceGeneric

    --print sourceGeneric
    print "---------------"
    --print source
    print $ encode (fmap convertPackage source)


convertPackage :: PackageDescription -> Package
convertPackage desc = def & Package.authors     .~ (splitOn "," $ DistPkgDesc.author desc)
                          & Package.bugReports  .~ (DistPkgDesc.bugReports desc)
                          & Package.copyright   .~ (DistPkgDesc.copyright desc)
                          & Package.description .~ (DistPkgDesc.description desc)
                          & Package.homepage    .~ (DistPkgDesc.homepage desc)
                          & Package.maintainers .~ (splitOn "," $ DistPkgDesc.maintainer desc)
                          & Package.synopsis    .~ (DistPkgDesc.synopsis desc)
                          & Package.tags        .~ (splitOn "," $ DistPkgDesc.category desc)
                          & Package.url         .~ (DistPkgDesc.pkgUrl desc)


--data Package = Package { name :: String 
--                       } deriving (Show, Generic)



