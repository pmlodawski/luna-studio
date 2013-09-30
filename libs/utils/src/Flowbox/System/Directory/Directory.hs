-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.Directory.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    doesDirectoryExist,
    copyDirectoryRecursive,
    getDirectoryRecursive,
    removeDirectoryRecursive,

    module System.Directory,
) where

import           Control.Applicative      
import qualified Data.List              as List
import qualified System.Directory       as Directory
import           System.Directory       hiding (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)

import           Flowbox.Prelude          
import qualified Flowbox.System.UniPath as UniPath
import           Flowbox.System.UniPath   (UniPath)



copyDirectoryRecursive :: UniPath -> UniPath -> IO ()
copyDirectoryRecursive usrc udst = do
    let 
        copyContent :: UniPath -> UniPath -> String -> IO ()
        copyContent s d name = do 
            let sname = UniPath.append name s 
                dname = UniPath.append name d
            isDir <- doesDirectoryExist sname
            if isDir 
                then do createDirectory $ UniPath.toUnixString dname 
                        copyDirectoryRecursive sname dname
                else do
                    isFile <- Directory.doesFileExist $ UniPath.toUnixString sname
                    if isFile 
                        then Directory.copyFile (UniPath.toUnixString sname) (UniPath.toUnixString dname)
                        else fail $ "Failed to copy '" ++ (UniPath.toUnixString sname) ++  "' not implmented record type."

    src <- UniPath.expand usrc
    dst <- UniPath.expand udst
    contents <- filter (`notElem` [".", ".."]) <$> Directory.getDirectoryContents (UniPath.toUnixString src)
    mapM_ (copyContent src dst) contents 


createDirectoryIfMissing :: Bool -> UniPath -> IO ()
createDirectoryIfMissing create_parents upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.createDirectoryIfMissing create_parents path


doesDirectoryExist :: UniPath -> IO Bool
doesDirectoryExist upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.doesDirectoryExist path


doesFileExist :: UniPath -> IO Bool
doesFileExist upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.doesFileExist path


getDirectoryRecursive :: UniPath -> IO [UniPath]
getDirectoryRecursive upath = do
    path  <- UniPath.expand upath
    isDir <- doesDirectoryExist path
    if isDir
        then do paths <- Directory.getDirectoryContents $ UniPath.toUnixString path
                let filtered = filter (/= ".") $ filter (/= "..") paths
                    upaths = map (\a -> UniPath.append a path) filtered
                children <- mapM getDirectoryRecursive upaths
                return $ List.concat children
        else return [path]


removeDirectoryRecursive :: UniPath -> IO ()
removeDirectoryRecursive upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.removeDirectoryRecursive path

