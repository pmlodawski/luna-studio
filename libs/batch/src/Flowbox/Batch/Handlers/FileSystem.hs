---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Applicative             
import qualified Control.Exception             as Exception
import qualified System.Directory              as Directory
import qualified System.IO                     as IO

import           Flowbox.Prelude                 
import qualified Flowbox.Batch.FileSystem.Item as Item
import           Flowbox.Batch.FileSystem.Item   (Item)
import qualified Flowbox.System.UniPath        as UniPath
import           Flowbox.System.UniPath          (UniPath)



ls :: UniPath -> IO [Item]
ls upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath

    paths <- Directory.getDirectoryContents path
    let upaths = map (\u -> UniPath.append u upath) paths
    items <- mapM stat upaths
    return items


stat :: UniPath -> IO Item
stat upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath

    isDir  <- Directory.doesDirectoryExist path
    if isDir 
        then return $ Item.Directory upath 0
        else do
            isFile <- Directory.doesFileExist path
            if isFile 
                then Exception.handle (\(_ :: Exception.SomeException) -> return $ Item.File upath (-1))
                     (do asize <- IO.withFile path IO.ReadMode IO.hFileSize
                         return $ Item.File upath $ fromInteger asize)
                else return $ Item.Other upath (-1)


mkdir :: UniPath -> IO ()
mkdir upath = do
     path <- UniPath.toUnixString <$> UniPath.expand upath
     Directory.createDirectory path


touch :: UniPath -> IO ()
touch upath = do 
    path <- UniPath.toUnixString <$> UniPath.expand upath
    IO.writeFile path ""


rm :: UniPath -> IO ()
rm upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath

    isDir  <- Directory.doesDirectoryExist path
    if isDir 
        then Directory.removeDirectoryRecursive path
        else do
            isFile <- Directory.doesFileExist path
            if isFile 
                then Directory.removeFile path
                else error "Could not remove object: Unsupported type."


cp :: UniPath -> UniPath -> IO ()
cp usrc udst = do
    src <- UniPath.toUnixString <$> UniPath.expand usrc
    dst <- UniPath.toUnixString <$> UniPath.expand udst

    isDir  <- Directory.doesDirectoryExist src
    if isDir 
        -- TODO [PM] : Implement copying of folders
        then putStrLn "Could not copy folder: Not Implemented. Sorry."
        else do
            isFile <- Directory.doesFileExist src
            if isFile 
                then Directory.copyFile src dst
                else error "Could not copy object: Unsupported type."
    

mv :: UniPath -> UniPath -> IO ()
mv usrc udst = do
    src <- UniPath.toUnixString <$> UniPath.expand usrc
    dst <- UniPath.toUnixString <$> UniPath.expand udst

    isDir  <- Directory.doesDirectoryExist src
    if isDir 
        then Directory.renameDirectory src dst
        else do
            isFile <- Directory.doesFileExist src
            if isFile 
                then Directory.renameFile src dst
                else error "Could not move object: Unsupported type."
