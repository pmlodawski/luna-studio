---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.RepoManager.Data.Repository where

import Data.Map as Map

import qualified Data.List                            as List
import qualified Data.Version                         as Version
import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Item.Config as Item
import           Flowbox.RepoManager.Data.Item.Family            (AvailableFamilies, InstalledFamilies)
import qualified Flowbox.RepoManager.Data.Item.Item   as Item
import qualified Flowbox.RepoManager.Data.Item.Name   as Item
import qualified System.Directory                     as Files
import qualified System.FilePath                      as Files   (pathSeparator)
import qualified Flowbox.RepoManager.Utils.Utils      as Utils   (concatPath)
import qualified Flowbox.RepoManager.VCS.VCS  as VCS
import qualified Flowbox.RepoManager.VCS.Type  as VCS
import qualified Flowbox.RepoManager.VCS.Git.Git  as Git
import qualified Text.Regex.Posix as Regex

data Repository = Repository { items :: Map Item.Name AvailableFamilies
                             , getVCS :: VCS.VCS
                             } deriving (Show)


data World = World { installed :: Map Item.Name InstalledFamilies
                   , selected  :: Map Item.Name InstalledFamilies
                   } deriving (Show)

type FileName = String

getRelevant :: [FilePath] -> [FilePath]
getRelevant files = files List.\\ [".git", "README.md", "..", "."]

buildRepository :: VCS.VCS -> IO Repository
buildRepository vcs = do let repoPath = VCS.localPath vcs
                         contents <- Files.getDirectoryContents repoPath
                         categories <- mapM (readCategory Map.empty repoPath) (getRelevant contents)
                         return Repository { items = List.foldl Map.union Map.empty categories
                                           , getVCS = vcs
                                           }

readCategory :: Map Item.Name AvailableFamilies -> FilePath -> FilePath -> IO (Map Item.Name AvailableFamilies)
readCategory repo repoPath categoryDir =  do let categoryPath = Utils.concatPath [repoPath, categoryDir]
                                             contents <- Files.getDirectoryContents categoryPath
                                             packList <- mapM (readPackage repo categoryPath) (getRelevant contents)
                                             return $ List.foldl Map.union Map.empty packList

readPackage :: Map Item.Name AvailableFamilies -> FilePath -> FilePath -> IO (Map Item.Name AvailableFamilies)
readPackage repo categoryPath directory = do let directoryPath = Utils.concatPath [categoryPath, directory]
                                             contents <- Files.getDirectoryContents directoryPath
                                             family  <- readPackageFamily (getRelevant contents) directoryPath
                                             return $ Map.insert directory family repo

readPackageFamily :: [FilePath] -> FilePath -> IO AvailableFamilies
readPackageFamily packageFiles directoryPath = do versionsList  <- mapM (readVersion directoryPath) packageFiles
                                                  return (Map.fromList versionsList)

readVersion :: FilePath ->  FilePath ->  IO (Version.Version, Item.Item)
readVersion directoryPath file = do item <- Item.loadItem $ Utils.concatPath [directoryPath, file]
                                    return (Item.version item, item)

initRepository :: VCS.VCS -> IO Repository
initRepository vcs = do let localPath = VCS.localPath vcs
                        exists <- Files.doesDirectoryExist $ Utils.concatPath [localPath, ".git"]
                        if exists
                            then buildRepository vcs
                            else Git.clone vcs >>= buildRepository

updateRepository :: VCS.VCS -> IO Repository
updateRepository vcs = Git.update vcs >>= buildRepository

searchRepository :: Repository -> String -> [Item.Name]
searchRepository repo expression = Map.keys $ Map.filterWithKey match repoItems
    where match key _value = key Regex.=~ expression :: Bool
          repoItems = items repo

--installPackage :: Item.Name
--installPackage name = 


