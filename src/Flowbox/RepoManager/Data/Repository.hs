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

build :: FilePath -> IO Repository
build repoPath = do contents <- Files.getDirectoryContents repoPath
                    categories <- mapM (category Map.empty) contents
                    return Repository {items = List.foldl Map.union Map.empty categories}

isProper :: String -> IO Bool
isProper file = do isDirectory <- Files.doesDirectoryExist file
                   return $ not (file `elem` [".git", "README.md", "..", "."]) && isDirectory

category :: Map Item.Name AvailableFamilies -> String -> IO (Map Item.Name AvailableFamilies)
category repo categoryDir = do proper <- isProper categoryDir
                               case proper of
                                    False -> return repo
                                    True  -> do contents <- Files.getDirectoryContents categoryDir
                                                packList <- mapM (package repo) contents 
                                                return $ List.foldl Map.union Map.empty packList

package :: Map Item.Name AvailableFamilies -> FilePath -> IO (Map Item.Name AvailableFamilies)
package repo directory = do proper <- isProper directory
                            case proper of
                                False -> return repo
                                True  -> do contents <- Files.getDirectoryContents directory
                                            let family = packageFamily contents
                                            return $ Map.insert directory family repo
                                     
packageFamily :: [FilePath] -> AvailableFamilies
packageFamily packageFiles = (Map.fromList $ List.map version packageFiles)

version :: FilePath ->  (Version.Version, Item.Item)
version file = let 
                 -- parse version file, build item
                 version' = Version.Version [1,2,3,4] ["ole"]
                 name' = file
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
                 in (version', item)
                     
                        

                             
