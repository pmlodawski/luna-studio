---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Flowbox.FileManager.Item where

import           System.PosixCompat.Files (FileStatus)
import qualified System.PosixCompat.Files as Files

import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.FileManager.FileSystem.Item     as Gen
import qualified Generated.Proto.FileManager.FileSystem.Item.Cls as Gen



toGen :: FilePath -> FileStatus -> Gen.Item
toGen name status = Gen.Item cls (encodePJ name) (Just size) (Just access) (Just modified) (Just statusChange) where
    cls | Files.isDirectory status   = Gen.Directory
        | Files.isRegularFile status = Gen.File
        | otherwise                  = Gen.Other
    size         = encodeP $ Files.fileSize         status
    access       = encodeP $ Files.accessTime       status
    modified     = encodeP $ Files.modificationTime status
    statusChange = encodeP $ Files.statusChangeTime status
