-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.System.Directory.Directory (
    copyDirectoryRecursive,
    copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesFileExist,
    doesDirectoryExist,
    getCurrentDirectory,
    getDirectoryRecursive,
    getTemporaryDirectory,
    getTmpDirectoryWithPrefix,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    renameDirectory,
    renameFile,
    setCurrentDirectory,
    touchFile,
    withTmpDirectory,
    module System.Directory,
) where

import           Control.Applicative
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List              as List
import           System.Directory       hiding (copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile, renameDirectory, renameFile, setCurrentDirectory)
import qualified System.Directory       as Directory
import qualified System.IO              as IO

import           Flowbox.Prelude        hiding (children)
import qualified Flowbox.System.Random  as Random
import           Flowbox.System.UniPath (UniPath)
import qualified Flowbox.System.UniPath as UniPath



copyDirectoryRecursive :: UniPath -> UniPath -> IO ()
copyDirectoryRecursive usrc udst = do
    let
        copyContent :: UniPath -> UniPath -> String -> IO ()
        copyContent s d name = do
            let sname = UniPath.append name s
                dname = UniPath.append name d
            isDir <- doesDirectoryExist sname
            if isDir
                then do createDirectory dname
                        contents <- listDirectory (UniPath.toUnixString sname)
                        mapM_ (copyContent sname dname) contents
                else do
                    isFile <- doesFileExist sname
                    if isFile
                        then copyFile sname dname
                        else fail $ "Failed to copy '" ++ UniPath.toUnixString sname ++  "' not implmented record type."

    src <- UniPath.expand usrc
    dst <- UniPath.expand udst
    let base     = UniPath.basePath src
        fileName = UniPath.fileName src
    copyContent base dst fileName


copyFile :: UniPath -> UniPath -> IO ()
copyFile usrc udst = do
    src <- UniPath.toUnixString <$> UniPath.expand usrc
    dst <- UniPath.toUnixString <$> UniPath.expand udst
    Directory.copyFile src dst


createDirectory :: UniPath -> IO ()
createDirectory upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.createDirectory path


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


getCurrentDirectory :: IO UniPath
getCurrentDirectory = UniPath.fromUnixString <$> Directory.getCurrentDirectory


getDirectoryRecursive :: UniPath -> IO [UniPath]
getDirectoryRecursive upath = do
    path  <- UniPath.expand upath
    isDir <- doesDirectoryExist path
    if isDir
        then do paths <- listDirectory $ UniPath.toUnixString path
                let upaths = map (`UniPath.append` path) paths
                children <- mapM getDirectoryRecursive upaths
                return $ List.concat children
        else return [path]


getTemporaryDirectory :: IO UniPath
getTemporaryDirectory = UniPath.fromUnixString <$> Directory.getTemporaryDirectory


getTmpDirectoryWithPrefix :: String -> IO UniPath
getTmpDirectoryWithPrefix prefix = do
    systemTmp <- getTemporaryDirectory
    guid      <- Random.newGUID
    return $ UniPath.append guid $ UniPath.append prefix systemTmp


listDirectory :: FilePath -> IO [FilePath]
listDirectory dirName = filter (`notElem` [".", ".."]) <$> Directory.getDirectoryContents dirName


removeDirectoryRecursive :: UniPath -> IO ()
removeDirectoryRecursive upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.removeDirectoryRecursive path


removeFile :: UniPath -> IO ()
removeFile upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.removeFile path


renameDirectory :: UniPath -> UniPath -> IO ()
renameDirectory usrc udst = do
    src <- UniPath.toUnixString <$> UniPath.expand usrc
    dst <- UniPath.toUnixString <$> UniPath.expand udst
    Directory.renameDirectory src dst


renameFile :: UniPath -> UniPath -> IO ()
renameFile usrc udst = do
    src <- UniPath.toUnixString <$> UniPath.expand usrc
    dst <- UniPath.toUnixString <$> UniPath.expand udst
    Directory.renameFile src dst


setCurrentDirectory :: UniPath -> IO ()
setCurrentDirectory upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.setCurrentDirectory path


touchFile :: UniPath -> IO ()
touchFile upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    IO.writeFile path ""


withTmpDirectory :: MonadIO m => String -> (UniPath -> m a) -> m a
withTmpDirectory prefix operation = do
    tmpDir <- liftIO $ getTmpDirectoryWithPrefix prefix
    liftIO $ createDirectoryIfMissing True tmpDir
    result <- operation tmpDir
    liftIO $ removeDirectoryRecursive tmpDir
    return result
