---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Flowbox.FileManager.LocalFileManager where

import           Control.Exception        (SomeException, catch)
import qualified System.Directory         as Directory
import qualified System.PosixCompat.Files as Files

import           Flowbox.Control.Error
import           Flowbox.FileManager.FileManager
import           Flowbox.FileManager.Item                        (toGen)
import           Flowbox.Prelude
import qualified Flowbox.System.Directory.Directory              as FDirectory
import qualified Flowbox.System.UniPath                          as UniPath
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.FileManager.FileSystem.Item     as Gen
import qualified Generated.Proto.FileManager.FileSystem.Item.Cls as Gen


data LocalFileManager = LocalFileManager


getFileStatus :: FilePath -> IO Gen.Item
getFileStatus name =
    catch (toGen name <$> Files.getFileStatus name) $ \(_ :: SomeException) -> do
        cls <- Directory.doesFileExist name >>= \case
            True  -> return Gen.File
            False -> Directory.doesDirectoryExist name >>= \case
                True  -> return Gen.Directory
                False -> return Gen.Other
        return $ Gen.Item cls (encodePJ name) Nothing Nothing Nothing Nothing


instance FileManager LocalFileManager () where
    stat            _   = safeLiftIO . getFileStatus
    uploadDirectory _ _ = return ()
    fetchDirectory  _ _ = return ()
    createDirectory _   = safeLiftIO . Directory.createDirectory
    directoryExists _   = safeLiftIO . Directory.doesDirectoryExist
    listDirectory   _ p = safeLiftIO $ mapM getFileStatus
                                   =<< Directory.getDirectoryContents p
    removeDirectory _   = safeLiftIO . Directory.removeDirectory
    copyDirectory   _ src dst = safeLiftIO $ FDirectory.copyDirectoryRecursive
                                     (UniPath.fromUnixString src)
                                     (UniPath.fromUnixString dst)
    moveDirectory   _ = safeLiftIO .: Directory.renameDirectory

    uploadFile      _ _ = return ()
    fetchFile       _ _ = return ()
    fileExists      _   = safeLiftIO . Directory.doesFileExist
    removeFile      _   = safeLiftIO . Directory.removeFile
    copyFile        _   = safeLiftIO .: Directory.copyFile
    moveFile        _   = safeLiftIO .: Directory.renameFile

    resolvePath     _ path = UniPath.toUnixString <$> UniPath.expand (UniPath.fromUnixString path)

