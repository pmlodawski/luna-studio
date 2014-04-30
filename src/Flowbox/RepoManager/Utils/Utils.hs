module Flowbox.RepoManager.Utils.Utils where

import           Flowbox.Prelude
import           Control.Monad
import qualified Data.List             as List
import qualified Data.List.Split       as Split
import qualified System.FilePath       as FilePath
import qualified System.Directory      as Directory
import qualified System.Directory.Tree as DirTree
import qualified Data.Foldable         as Foldable
import qualified Data.Maybe            as Maybe
import qualified Flowbox.RepoManager.Data.Version as Version

concatPath :: [String] -> String
concatPath directories = List.intercalate [FilePath.pathSeparator] directories

getDirectories :: FilePath -> IO [FilePath]
getDirectories path = filterPath isDirectory path
    where isDirectory d = Directory.doesDirectoryExist $ FilePath.joinPath [path, d]

getFiles :: FilePath -> IO [FilePath]
getFiles path = filterPath isFile path
    where isFile f = Directory.doesFileExist $ FilePath.joinPath [path, f]

filterPath :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
filterPath f path = do contents <- Directory.getDirectoryContents path
                       filterM f contents

--                                    files       dirs
splitDirsAndFiles :: FilePath -> IO ([FilePath], [FilePath])
splitDirsAndFiles path = (,) <$> getFiles path <*> getDirectories path

--                  categories  package filename
type PackageFile = ([String]  , String, String)

listFilesInPath :: FilePath -> IO [FilePath]
listFilesInPath path = do _ DirTree.:/ tree <- DirTree.readDirectoryWith return path
                          return $ Maybe.mapMaybe getName $ DirTree.flattenDir tree

getName :: DirTree.DirTree String -> Maybe String
getName (DirTree.File _ name) = Just name
getName _                     = Nothing

-- example file path "portagetree/cata/catb/catc/pkga/pkga-1.2.3.build"
relativePathToPackageFile :: FilePath -> PackageFile
relativePathToPackageFile path = (tail . reverse $ category, packageName, buildFileName)
    where reversedSplitUpPath             = reverse $ Split.splitOn "/" path
          buildFileName : pkgCategoryPath = reversedSplitUpPath
          packageName : category          = pkgCategoryPath

listLocalAvailablePackages :: FilePath -> IO [String]
listLocalAvailablePackages path = do files <- listFilesInPath path
                                     let packages  = map relativePathToPackageFile files
                                         packages' = map (_1 %~ FilePath.joinPath) packages
                                         pkgNames  = map (\(c,n,_) -> c ++ "/" ++ n) packages'
                                     return pkgNames

listAvailablePackageVersions :: FilePath -> IO [Version.Version]
listAvailablePackageVersions dir = do scripts <- listPackageScripts dir
                                      return $ map fileNameToVersion scripts
    where fileNameToVersion = Version.parseVersion . tail . dropWhile (/= '-') . FilePath.dropExtension

listPackageScripts :: FilePath -> IO [FilePath]
listPackageScripts dir = do files <- Directory.getDirectoryContents dir
                            let scripts = filter (\x -> FilePath.takeExtension x == ".config") files
                            return scripts


withDirectory :: FilePath -> IO a -> IO a
withDirectory dir action = do currentDir <- Directory.getCurrentDirectory
                              Directory.setCurrentDirectory dir
                              result <- action
                              Directory.setCurrentDirectory currentDir
                              return result