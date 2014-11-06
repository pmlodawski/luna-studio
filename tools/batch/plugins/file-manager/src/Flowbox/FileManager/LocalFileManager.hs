---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.FileManager.LocalFileManager where

import           Flowbox.Control.Error
import           Flowbox.FileManager.FileManager
import           Flowbox.Prelude                    hiding (Context)
import qualified Flowbox.System.Directory.Directory as FDirectory
import qualified Flowbox.System.UniPath             as UniPath
import qualified System.Directory                   as Directory



data LocalFileManager = LocalFileManager


instance FileManager LocalFileManager () where
    uploadDirectory _ _ = return ()
    fetchDirectory  _ _ = return ()
    createDirectory _ = safeLiftIO . Directory.createDirectory
    directoryExists _ = safeLiftIO . Directory.doesDirectoryExist
    listDirectory   _ = safeLiftIO . Directory.getDirectoryContents
    removeDirectory _ = safeLiftIO . Directory.removeDirectory
    copyDirectory   _ src dst = safeLiftIO $
        FDirectory.copyDirectoryRecursive (UniPath.fromUnixString src) (UniPath.fromUnixString dst)
    moveDirectory   _ = safeLiftIO .: Directory.renameDirectory

    uploadFile      _ _ = return ()
    fetchFile       _ _ = return ()
    fileExists      _ = safeLiftIO . Directory.doesFileExist
    removeFile      _ = safeLiftIO . Directory.removeFile
    copyFile        _ = safeLiftIO .: Directory.copyFile
    moveFile        _ = safeLiftIO .: Directory.renameFile

    resolvePath     _ path = UniPath.toUnixString <$> UniPath.expand (UniPath.fromUnixString path)

