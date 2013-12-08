---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Distribution.Client.List where

import qualified Data.Map as Map

import qualified Flowbox.Config.Config                      as Config
import qualified Flowbox.Distribution.Package.Package       as Package
import qualified Flowbox.Distribution.Package.PackageFamily as PackageFamily
import qualified Flowbox.Distribution.Package.PackageIndex  as PackageIndex
import           Flowbox.Prelude

list :: [String] -> IO ()
list patterns = do
    let name = patterns !! 0
    cfg <- Config.load
    srcPkgIdx  <- PackageIndex.readSrcPkgIdx cfg
    instPkgIdx <- PackageIndex.readInstPkgIdx cfg
    let srcPkgs    = PackageIndex.searchByNameSubstring srcPkgIdx name
        instPkgs   = PackageIndex.searchByNameSubstring instPkgIdx name
        srcPkgMap  = PackageIndex.partitionByName srcPkgs
        instPkgMap = PackageIndex.partitionByName instPkgs
        pkgMap     = PackageIndex.combinePkgMaps srcPkgMap instPkgMap
        pkgFMap = Map.map PackageFamily.mk pkgMap

    mapM_ PackageFamily.print $ Map.elems pkgFMap
