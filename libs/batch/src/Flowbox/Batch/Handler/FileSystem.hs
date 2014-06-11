---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Batch.Handler.FileSystem (
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
import qualified Control.Exception                  as Exception
import qualified Data.List                          as List
import qualified Flowbox.System.Directory.Directory as Directory
import qualified System.IO                          as IO

import           Flowbox.Batch.FileSystem.Item (Item)
import qualified Flowbox.Batch.FileSystem.Item as Item
import           Flowbox.Prelude
import           Flowbox.System.UniPath        (UniPath)
import qualified Flowbox.System.UniPath        as UniPath



ls :: UniPath -> IO [Item]
ls upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath

    paths <- Directory.getDirectoryContents path
    let upaths = List.sort $ map (\u -> UniPath.append u upath) paths
    mapM stat upaths


stat :: UniPath -> IO Item
stat upath = do
    path  <- UniPath.expand upath
    isDir <- Directory.doesDirectoryExist path
    if isDir
        then return $ Item.Directory path 0
        else do
            isFile <- Directory.doesFileExist path
            if isFile
                then Exception.handle (\(_ :: Exception.SomeException) -> return $ Item.File path (-1))
                     (do asize <- IO.withFile (UniPath.toUnixString path) IO.ReadMode IO.hFileSize
                         return $ Item.File path $ fromInteger asize)
                else return $ Item.Other path (-1)


mkdir :: UniPath -> IO ()
mkdir = Directory.createDirectoryIfMissing True


touch :: UniPath -> IO ()
touch = Directory.touchFile


rm :: UniPath -> IO ()
rm upath = do
    path  <- UniPath.expand upath
    isDir <- Directory.doesDirectoryExist path
    if isDir
        then Directory.removeDirectoryRecursive path
        else do
            isFile <- Directory.doesFileExist path
            if isFile
                then Directory.removeFile path
                else error "Could not remove object: Unsupported type."


cp :: UniPath -> UniPath -> IO ()
cp = Directory.copyDirectoryRecursive


mv :: UniPath -> UniPath -> IO ()
mv usrc udst = do
    src   <- UniPath.expand usrc
    dst   <- UniPath.expand udst
    isDir <- Directory.doesDirectoryExist src
    if isDir
        then Directory.renameDirectory src dst
        else do
            isFile <- Directory.doesFileExist src
            if isFile
                then Directory.renameFile src dst
                else error "Could not move object: Unsupported type."
