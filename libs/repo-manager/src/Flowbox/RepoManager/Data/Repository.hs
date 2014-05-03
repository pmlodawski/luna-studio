---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.RepoManager.Data.Repository where

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.List                            as List
import qualified Flowbox.RepoManager.Data.Version     as Version
import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Package.Config  as Package
import qualified Flowbox.RepoManager.Data.Package.Family  as Family
import qualified Flowbox.RepoManager.Data.Package.Package as Package
import qualified Flowbox.RepoManager.Data.RepoConfig  as RepoConfig
import qualified Flowbox.RepoManager.Utils.Utils      as Utils
import qualified System.Directory                     as Directory
import qualified System.FilePath                      as FilePath
import qualified Flowbox.RepoManager.Utils.Utils      as Utils   (concatPath)
import qualified Flowbox.RepoManager.VCS.VCS          as VCS
import qualified Text.Regex.Posix                     as Regex
import qualified Network.URI                          as URI

data Repository a = Repository { config   :: RepoConfig.RepoConfig
                               , packages :: Map String [Package.Package]
                               , getVCS   :: a
                               } deriving (Show)

localRepoPath :: VCS.VCS a => Repository a -> FilePath
localRepoPath = VCS.localPath . getVCS

buildRepository :: VCS.VCS a => a -> RepoConfig.RepoConfig -> IO (Repository a)
buildRepository vcs conf = do let repoPath = VCS.localPath vcs
                              packagesNames <- Utils.withDirectory repoPath $ Utils.listLocalAvailablePackages "."
                              packages <- mapM (readPackage repoPath) packagesNames
                              return $ Repository conf (Map.fromList $ zip packagesNames packages) vcs

readPackage :: FilePath -> String -> IO [Package.Package]
readPackage repoPath qualifiedPkgName = Utils.withDirectory repoPath $ do
    buildFiles <- Utils.listPackageScripts qualifiedPkgName
    mapM Package.readBuildFile $ map (\x -> FilePath.joinPath [qualifiedPkgName, x]) buildFiles


readPackageFamily :: FilePath -> String -> IO Family.PackageFamily
readPackageFamily repoPath qualifiedPkgName = Family.PackageFamily <$> pure qualifiedPkgName <*> versionMap
    where tupWithVersion package = (Package._version package, package)
          versionMap = do packageList <- readPackage repoPath qualifiedPkgName
                          return $ Map.fromList $ map tupWithVersion packageList


--buildRepository :: VCS.VCS a => a -> IO (Repository a)
--buildRepository vcs = do let repoPath = VCS.localPath vcs
--                         contents <- Files.getDirectoryContents $ show repoPath
--                         categories <- mapM (readCategory Map.empty (show repoPath)) (getRelevant contents)
--                         return Repository { Packages = List.foldl' Map.union Map.empty categories
--                                           , getVCS = vcs
--                                           }

--readCategory :: Map String AvailableFamilies -> FilePath -> FilePath -> IO (Map String AvailableFamilies)
--readCategory repo repoPath categoryDir =  do let categoryPath = Utils.concatPath [repoPath, categoryDir]
--                                             contents <- Files.getDirectoryContents categoryPath
--                                             packList <- mapM (readPackage repo categoryPath) (getRelevant contents)
--                                             return $ List.foldl' Map.union Map.empty packList

--readPackage :: Map String AvailableFamilies -> FilePath -> FilePath -> IO (Map String AvailableFamilies)
--readPackage repo categoryPath directory = do let directoryPath = Utils.concatPath [categoryPath, directory]
--                                             contents <- Files.getDirectoryContents directoryPath
--                                             family  <- readPackageFamily (getRelevant contents) directoryPath
--                                             return $ Map.insert directory family repo

--readPackageFamily :: [FilePath] -> FilePath -> IO AvailableFamilies
--readPackageFamily packageFiles directoryPath = do versionsList  <- mapM (readVersion directoryPath) packageFiles
--                                                  return (Map.fromList versionsList)

--readVersion :: FilePath ->  FilePath ->  IO (Version.Version, Package.Package)
--readVersion directoryPath file = do Package <- Package.loadPackage $ Utils.concatPath [directoryPath, file]
--                                    return (Package.version Package, Package)

--initRepository :: VCS.VCS a => a -> IO (Repository a)
--initRepository vcs = do let localPath = VCS.localPath vcs
--                        exists <- Files.doesDirectoryExist $ Utils.concatPath [show localPath, ".git"]
--                        if exists
--                            then buildRepository vcs
--                            else VCS.clone vcs >> buildRepository vcs

--updateRepository :: VCS.VCS a => a -> IO (Repository a)
--updateRepository vcs = VCS.pull vcs >> buildRepository vcs

searchRepository :: Repository a -> String -> [String]
searchRepository repo expression = Map.keys $ Map.filterWithKey match repoPackages
    where match key _value = key Regex.=~ expression :: Bool
          repoPackages = packages repo
