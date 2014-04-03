{-# LANGUAGE ScopedTypeVariables         #-}
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
import qualified System.FilePath as Files (pathSeparator)
import qualified Data.Version as Version
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Flowbox.RepoManager.Data.Item.Item as Item
import qualified Flowbox.RepoManager.Data.Dependency as Dependency
import qualified Flowbox.RepoManager.Data.Environment as URI
import qualified Flowbox.RepoManager.Data.Item.Config as Item
import qualified Data.String as String (fromString)

data Repository = Repository { items :: Map Item.Name AvailableFamilies
                             } deriving (Show)


data World = World { installed :: Map Item.Name InstalledFamilies
                   , selected  :: Map Item.Name InstalledFamilies
                   } deriving (Show)

type FileName = String

build :: FilePath -> IO Repository
build repoPath = do contents <- Files.getDirectoryContents repoPath
                    --print contents
                    categories <- mapM (category Map.empty repoPath) (proper contents)
                    return Repository {items = List.foldl Map.union Map.empty categories}

proper :: [FilePath] -> [FilePath]
proper files = files List.\\ [".git", "README.md", "..", "."]

category :: Map Item.Name AvailableFamilies -> FilePath -> FilePath -> IO (Map Item.Name AvailableFamilies)
category repo repoPath categoryDir =  do let categoryPath = concat [repoPath, [Files.pathSeparator], categoryDir]
                                         --print ("cat: " ++ categoryPath)
                                         contents <- Files.getDirectoryContents categoryPath
                                         packList <- mapM (package repo categoryPath) (proper contents)
                                         return $ List.foldl Map.union Map.empty packList

package :: Map Item.Name AvailableFamilies -> FilePath -> FilePath -> IO (Map Item.Name AvailableFamilies)
package repo categoryPath directory = do let directoryPath = concat [categoryPath, [Files.pathSeparator], directory]
                                         --print ("dir: " ++ directoryPath)
                                         contents <- Files.getDirectoryContents directoryPath
                                         family <- packageFamily (proper contents) directoryPath
                                         return $ Map.insert directory family repo

packageFamily :: [FilePath] -> FilePath -> IO AvailableFamilies
packageFamily packageFiles directoryPath = do versionsList  <- mapM (version directoryPath) packageFiles
                                              --print "versions:"
                                              --print versionsList
                                              return (Map.fromList versionsList)


version :: FilePath ->  FilePath ->  IO (Version.Version, Item.Item)
version directoryPath file = do item <- Item.loadItem $ concat [directoryPath, [Files.pathSeparator], file]
                                return (Item.version item, item)                 
              
                     
                        

                             
