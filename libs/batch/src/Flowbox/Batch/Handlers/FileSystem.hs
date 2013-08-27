---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Handlers.FileSystem (
    ls,
    stat,

    mkdir,
    touch,
    rm,
    cp,
    mv,
) 
where

import qualified System.Directory              as Directory
import qualified System.IO                     as IO

import           Flowbox.Batch.FileSystem.Item   (Item(..))
import qualified Flowbox.System.UniPath        as UniPath
import           Flowbox.System.UniPath          (UniPath)


handleFSObject :: UniPath -> (FilePath -> IO a) -> (FilePath -> IO a) -> IO a
handleFSObject upath dirOperation fileOperation = do 
    let apath = UniPath.toUnixString upath

    isDir  <- Directory.doesDirectoryExist apath
    if isDir 
        then dirOperation apath
        else do
            isFile <- Directory.doesFileExist apath
            if isFile 
                then fileOperation apath
                else error "Unsupported file system object type."

ls :: UniPath -> IO [Item]
ls upath = do
    paths <- Directory.getDirectoryContents (UniPath.toUnixString upath)
    let upaths = map UniPath.fromUnixString paths
    items <- mapM stat upaths
    return items


stat :: UniPath -> IO Item
stat upath = do
    let apath = UniPath.toUnixString upath

    isDir  <- Directory.doesDirectoryExist apath
    if isDir 
        then return $ Directory upath 0
        else do
            isFile <- Directory.doesFileExist apath
            if isFile 
                then do
                    asize <- IO.withFile apath IO.ReadMode IO.hFileSize
                    return $ File upath $ fromInteger asize
                else return $ Other upath (-1)


mkdir :: UniPath -> IO ()
mkdir upath = Directory.createDirectory (UniPath.toUnixString upath)


touch :: UniPath -> IO ()
touch upath = IO.writeFile (UniPath.toUnixString upath) ""


rm :: UniPath -> IO ()
rm upath = do
    let apath = UniPath.toUnixString upath

    isDir  <- Directory.doesDirectoryExist apath
    if isDir 
        then Directory.removeDirectoryRecursive apath
        else do
            isFile <- Directory.doesFileExist apath
            if isFile 
                then Directory.removeFile apath
                else error "Could not remove object: Unsupported type."


cp :: UniPath -> UniPath -> IO ()
cp usrc udst = do
    let asrc = UniPath.toUnixString usrc
        adst = UniPath.toUnixString udst

    isDir  <- Directory.doesDirectoryExist asrc
    if isDir 
        -- TODO [PM] : Implement copying of folders
        then putStrLn "Could not copy folder: Not Implemented. Sorry."
        else do
            isFile <- Directory.doesFileExist asrc
            if isFile 
                then Directory.copyFile asrc adst
                else error "Could not copy object: Unsupported type."
    

mv :: UniPath -> UniPath -> IO ()
mv usrc udst = do
    let asrc = UniPath.toUnixString usrc
        adst = UniPath.toUnixString udst

    isDir  <- Directory.doesDirectoryExist asrc
    if isDir 
        then Directory.renameDirectory asrc adst
        else do
            isFile <- Directory.doesFileExist asrc
            if isFile 
                then Directory.renameFile asrc adst
                else error "Could not move object: Unsupported type."
