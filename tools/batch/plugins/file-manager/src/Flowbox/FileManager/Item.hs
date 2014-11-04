---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.FileManager.Item where

import           System.PosixCompat.Files (FileStatus)
import qualified System.PosixCompat.Files as Files

import           Flowbox.Prelude
import qualified Generated.Proto.FileManager.FileSystem.Item     as Gen
import qualified Generated.Proto.FileManager.FileSystem.Item.Cls as Gen



toGen :: FileStatus -> Gen.Item
toGen status = Gen.Item cls (Just size) (Just access) (Just modified) (Just statusChange) where
    cls | Files.isDirectory status   = Gen.Directory
        | Files.isRegularFile status = Gen.File
        | otherwise                  = Gen.Other
    size         = fromIntegral            $ Files.fileSize         status
    access       = fromIntegral $ fromEnum $ Files.accessTime       status
    modified     = fromIntegral $ fromEnum $ Files.modificationTime status
    statusChange = fromIntegral $ fromEnum $ Files.statusChangeTime status
