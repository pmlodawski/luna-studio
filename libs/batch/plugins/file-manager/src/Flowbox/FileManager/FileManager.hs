---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.FileManager.FileManager where

import Flowbox.Bus.RPC.RPC (RPC)
import Flowbox.Prelude



class FileManager fm ctx where
    uploadDirectory :: fm -> FilePath -> RPC ctx IO ()
    fetchDirectory  :: fm -> FilePath -> RPC ctx IO ()
    createDirectory :: fm -> FilePath -> RPC ctx IO ()
    directoryExists :: fm -> FilePath -> RPC ctx IO Bool
    listDirectory   :: fm -> FilePath -> RPC ctx IO [FilePath]
    removeDirectory :: fm -> FilePath -> RPC ctx IO ()
    copyDirectory   :: fm -> FilePath -> FilePath -> RPC ctx IO ()
    moveDirectory   :: fm -> FilePath -> FilePath -> RPC ctx IO ()

    uploadFile      :: fm -> FilePath -> RPC ctx IO ()
    fetchFile       :: fm -> FilePath -> RPC ctx IO ()
    fileExists      :: fm -> FilePath -> RPC ctx IO Bool
    removeFile      :: fm -> FilePath -> RPC ctx IO ()
    copyFile        :: fm -> FilePath -> FilePath -> RPC ctx IO ()
    moveFile        :: fm -> FilePath -> FilePath -> RPC ctx IO ()

    resolvePath     :: fm -> FilePath -> RPC ctx IO FilePath

