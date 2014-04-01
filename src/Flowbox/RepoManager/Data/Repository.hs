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

--build repoPath = do files <- Files.getDirectoryContents repoPath
--                    let cat = List.foldl category Repository {} files
--                    return ()

isProper :: String -> Bool
isProper file = not (file `elem` [".git", "README.md", "..", "."])


category :: IO (IO (Map Item.Name AvailableFamilies)) -> String -> IO (IO (Map Item.Name AvailableFamilies))
category repo categoryDir = case isProper categoryDir of
                                    False -> repo
                                    True  -> let 
                                        
                                                 folder = \m1 m2 -> Map.union <$> m1 <*> m2
                                                 packList = List.map <$> package <$> repo <*> Files.getDirectoryContents categoryDir
                                                 packeges = List.foldl folder (return Map.empty) <$> packList
                                             in packeges
                                        
                                        




package :: IO (Map Item.Name AvailableFamilies) -> FilePath -> IO (Map Item.Name AvailableFamilies)
package repo directory = case isProper directory of
                            False -> repo
                            True  -> Map.insert directory <$> (package' <$> Files.getDirectoryContents directory) <*> repo
                                     
package' :: [FilePath] -> Map Version.Version Item.Item
package' packageFiles = (Map.fromList $ List.map version packageFiles)

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
                     
                        

                             
