---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Repository where

import Data.Map as Map

import           Flowbox.Prelude
import           Flowbox.RepoManager.Data.Item.Family (InstalledFamilies, AvailableFamilies)
import qualified Flowbox.RepoManager.Data.Item.Name             as Item
import qualified System.Directory as Files
import qualified Data.Version as Version
import qualified Data.List as List
import qualified Flowbox.RepoManager.Data.Item.Item as Item
import qualified Flowbox.RepoManager.Data.Dependency as Dependency
import qualified Flowbox.RepoManager.Data.Environment as URI

data Repository = Repository { items :: Map Item.Name AvailableFamilies
                             } deriving (Show)


data World = World { installed :: Map Item.Name InstalledFamilies
                   , selected  :: Map Item.Name InstalledFamilies
                   } deriving (Show)

type FileName = String

build repoPath = do files <- Files.getDirectoryContents repoPath
                    let cat = List.foldl category Repository {} files
                    return ()

isProper :: String -> Bool
isProper file = not (file `elem` [".git", "README.md", "..", "."])

--category :: FileName -> FileName
category :: FilePath -> Map Item.Name AvailableFamilies -> Map Item.Name AvailableFamilies
category categoryDir repo = case isProper categoryDir of
                                    False -> repo
                                    True  -> do files <- Files.getDirectoryContents categoryDir
                                                Map.insert (List.foldl package Map.empty files)

package :: FilePath -> Map Item.Name AvailableFamilies -> Map Item.Name AvailableFamilies
package packageDir repo = case isProper packageDir of
                            False -> repo
                            True  -> 
                                     do packageFiles <- Files.getDirectoryContents packageDir
                                        let newPackage = List.foldl versions Map.empty packageFiles
                                        Map.insert packageDir newPackage repo

versions  :: FilePath -> Map Version.Version Item.Item
versions  file versions' = let version' = Version.versionBranch [1,2,3,4]
                               name' = "Item.Name"
                               source' = Map.singleton "x86" (URI.Local "local/uri")
                               dependancies' = [Dependency.Dependency { Dependency.name = "dependancy1"
                                                                      , Dependency.constraints = []
                                                                      }]
                               item = Item.Item { Item.name            = name'
                                                , Item.version         = version'
                                                , Item.source          = source'
                                                , Item.installScript   = ["script"]
                                                , Item.uninstallScript = ["script"]
                                                , Item.dependencies    = dependancies'
                                                }
                           in Map.insert $ name' Map.singleton name' item versions'
                        

                             
