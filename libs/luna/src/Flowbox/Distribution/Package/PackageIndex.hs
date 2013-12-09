---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Distribution.Package.PackageIndex where

import           Data.Char                        (toLower)
import qualified Data.List                        as List
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Monoid
import qualified Data.Set                         as Set
import qualified Distribution.Client.Config       as CabalConf
import qualified Distribution.Client.IndexUtils   as IndexUtils
import qualified Distribution.Client.PackageIndex as SourcePackageIndex
import           Distribution.Client.Sandbox      as Sandbox
import qualified Distribution.Client.Setup        as Setup
import qualified Distribution.Client.Types        as CliTypes
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import           Distribution.Verbosity           as Verbosity

import           Control.Arrow                        ((***))
import           Control.Monad                        (join)
import           Flowbox.Config.Config                (Config)
import qualified Flowbox.Distribution.CabalConversion as CabalConversion
import qualified Flowbox.Distribution.Config          as PkgConfig
import           Flowbox.Distribution.Package.Package (Package)
import qualified Flowbox.Distribution.Package.Package as Package
import           Flowbox.Prelude

import Debug.Trace

data PackageIndex = InstalledPackageIndex InstalledPackageIndex.PackageIndex
                  | SourcePackageIndex   (SourcePackageIndex.PackageIndex CliTypes.SourcePackage)
                  deriving (Show)


readSrcPkgIdx :: Config -> IO PackageIndex
readSrcPkgIdx cfg = do
    cabalCfg <- PkgConfig.readCabalCfg cfg
    let globalFlags  = PkgConfig.defaultGlobalFlags cfg
        globalFlags' = CabalConf.savedGlobalFlags cabalCfg `mappend` globalFlags
        repos        = Setup.globalRepos globalFlags'
    sourcePkgDb <- IndexUtils.getSourcePackages Verbosity.normal repos
    return $ SourcePackageIndex (CliTypes.packageIndex sourcePkgDb)


readInstPkgIdx :: Config -> IO PackageIndex
readInstPkgIdx cfg = do
    cabalCfg <- PkgConfig.readCabalCfg cfg
    let pkgDBs      = PkgConfig.localPkgStack cfg
        configFlags = CabalConf.savedConfigureFlags cabalCfg
    (comp, _, conf)   <- Sandbox.configCompilerAux' configFlags
    installedPkgIndex <- IndexUtils.getInstalledPackages Verbosity.normal comp pkgDBs conf
    return $ InstalledPackageIndex installedPkgIndex


searchByNameSubstring :: PackageIndex -> String -> [Package]
searchByNameSubstring idx pattern = case idx of
    InstalledPackageIndex pidx -> map CabalConversion.convertInstPackage $ InstalledPackageIndex.searchByNameSubstring pidx pattern
    SourcePackageIndex    pidx -> map CabalConversion.convertSrcPackage  $ concatMap snd (SourcePackageIndex.searchByNameSubstring pidx pattern)


allPackages :: PackageIndex -> [Package]
allPackages idx = case idx of
    InstalledPackageIndex pidx -> map CabalConversion.convertInstPackage $ InstalledPackageIndex.allPackages pidx
    SourcePackageIndex    pidx -> map CabalConversion.convertSrcPackage  $ SourcePackageIndex.allPackages pidx

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



combinePkgMaps :: Map String [Package] -> Map String [Package] -> Map String ([Package], [Package])
combinePkgMaps srcPkgMap instPkgMap = Map.mapWithKey insert allSrcMap
    where allSrcMap   = (Map.union srcPkgMap (Map.map (\_ -> []) instPkgMap))
          insert k el = (el, Map.findWithDefault [] k instPkgMap)


-- BUGREPORT[wd] : following functions take whole RAM.

--combinePkgMaps :: Map String [Package] -> Map String [Package] -> Map String ([Package], [Package])
--combinePkgMaps srcPkgMap instPkgMap = Set.foldr finsert mempty keys
--    where keys            = Set.union (Map.keysSet srcPkgMap) (Map.keysSet instPkgMap)
--          findPkgs key    = join (***) (Map.findWithDefault [] key) (srcPkgMap,instPkgMap)
--          finsert key map = Map.insert key (findPkgs key) map

--combinePkgMaps :: Map String [Package] -> Map String [Package] -> Map String ([Package], [Package])
--combinePkgMaps srcPkgMap instPkgMap = Map.fromList $ zip keys pkgs
--    where keys            = Set.toList $ Set.union (Map.keysSet srcPkgMap) (Map.keysSet instPkgMap)
--          pkgs            = map findPkgs keys
--          findPkgs key    = join (***) (Map.findWithDefault [] key) (srcPkgMap,instPkgMap)


--combinePkgMaps :: Map String [Package] -> Map String [Package] -> Map String ([Package], [Package])
--combinePkgMaps srcPkgMap instPkgMap = Map.fromList $ zip keys pkgs
--    where keys            = Set.toList $ Map.keysSet srcPkgMap
--          pkgs            = map findPkgs keys
--          findPkgs key    = join (***) (Map.findWithDefault [] key) (srcPkgMap,instPkgMap)
