---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Distribution.Client.List where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as ByteString
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.String          (fromString)
import           System.IO            (stdout)

import qualified Data.ByteString.Char8                        as Char8
import qualified Flowbox.Config.Config                        as Config
import           Flowbox.Distribution.Package.PackageFamily   (PackageFamily)
import qualified Flowbox.Distribution.Package.PackageFamily   as PackageFamily
import qualified Flowbox.Distribution.Package.PackageIndex    as PackageIndex
import           Flowbox.Prelude                              hiding (index, simple)
import qualified Flowbox.System.Console.StyledText.StyledText as StyledText
import qualified Text.Doc.Markup                              as Markup

list :: Bool -> [String] -> IO ()
list simple pats = do
    pkgFams <- Map.elems <$> getPkgFMap pats
    let output = format pkgFams
        format = if simple then map $ fromString . (view PackageFamily.name)
                           else map $ (++ "\n") . PackageFamily.styleShow
    mapM_ StyledText.print output


listJSON :: Bool -> Bool -> [String] -> IO ()
listJSON html simple pats = do
    pkgFams <- Map.elems <$> getPkgFMap pats
    let output = format pkgFams
        format = if simple then JSON.encode . (map $ view PackageFamily.name)
                           else case html of
                                False -> JSON.encode
                                True -> JSON.encode . (map $ \pgkF -> pgkF & PackageFamily.description %~ (\crap -> either show (Char8.unpack . ByteString.toStrict) (Markup.parse crap)))
    ByteString.hPut stdout output
    putStrLn ""


getPkgFMap :: [String] -> IO (Map String PackageFamily)
getPkgFMap pats = do
    cfg        <- Config.load
    srcPkgIdx  <- PackageIndex.readSrcPkgIdx cfg
    instPkgIdx <- PackageIndex.readInstPkgIdx cfg
    let matchingPackages search index = [ pkg | pat <- pats, pkg <- search index pat ]
        query      = if null pats then PackageIndex.allPackages
                                  else matchingPackages PackageIndex.searchByNameSubstring
        srcPkgs    = query srcPkgIdx
        instPkgs   = query instPkgIdx
        srcPkgMap  = PackageIndex.partitionByName srcPkgs
        instPkgMap = PackageIndex.partitionByName instPkgs
        pkgMap     = PackageIndex.combinePkgMaps srcPkgMap instPkgMap
        pkgFMap    = Map.map PackageFamily.mk pkgMap
    return pkgFMap
