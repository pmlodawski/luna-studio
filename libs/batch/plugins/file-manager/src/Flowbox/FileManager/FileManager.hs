---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.FileManager.FileManager where

import           Data.ByteString.Lazy                        (ByteString)

import           Flowbox.Bus.RPC.RPC                         (RPC)
import           Flowbox.Prelude
import qualified Generated.Proto.FileManager.FileSystem.Item as Gen



class FileManager fm ctx where
    stat            :: fm -> FilePath -> RPC ctx IO Gen.Item

    createDirectory       :: fm -> FilePath -> RPC ctx IO ()
    directoryExists       :: fm -> FilePath -> RPC ctx IO Bool
    listDirectory         :: fm -> FilePath -> RPC ctx IO [Gen.Item]
    removeDirectory       :: fm -> FilePath -> RPC ctx IO ()
    copyDirectory         :: fm -> FilePath -> FilePath -> RPC ctx IO ()
    moveDirectory         :: fm -> FilePath -> FilePath -> RPC ctx IO ()
    remoteUploadDirectory :: fm -> FilePath -> RPC ctx IO ()
    remoteFetchDirectory  :: fm -> FilePath -> RPC ctx IO ()

    download              :: fm -> FilePath -> RPC ctx IO ByteString
    upload                :: fm -> FilePath -> ByteString -> RPC ctx IO ()
    fileExists            :: fm -> FilePath -> RPC ctx IO Bool
    removeFile            :: fm -> FilePath -> RPC ctx IO ()
    copyFile              :: fm -> FilePath -> FilePath -> RPC ctx IO ()
    moveFile              :: fm -> FilePath -> FilePath -> RPC ctx IO ()
    remoteUploadFile      :: fm -> FilePath -> RPC ctx IO ()
    remoteFetchFile       :: fm -> FilePath -> RPC ctx IO ()

    resolvePath           :: fm -> FilePath -> RPC ctx IO FilePath

