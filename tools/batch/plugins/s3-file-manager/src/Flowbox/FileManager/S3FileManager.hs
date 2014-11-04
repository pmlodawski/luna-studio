---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.FileManager.S3FileManager where

import qualified Flowbox.AWS.S3.Directory        as Directory
import qualified Flowbox.AWS.S3.File             as File
import qualified Flowbox.AWS.S3.S3               as S3
import qualified Flowbox.FileManager.Context     as Context
import           Flowbox.FileManager.FileManager
import           Flowbox.FileManager.Item        (toGen)
import           Flowbox.Prelude                 hiding (Context)


data S3FileManager = S3FileManager



instance FileManager S3FileManager S3.S3Env where
    uploadDirectory _ = Context.run . Directory.upload "."
    fetchDirectory  _ = Context.run . Directory.fetch  "."
    createDirectory _ = Context.run . Directory.create
    directoryExists _ = Context.run . Directory.exists
    listDirectory   _ = fmap (map toGen)
                      . Context.run . Directory.getContents
    removeDirectory _ = Context.run . Directory.remove
    copyDirectory   _ = Context.run .: Directory.copy
    moveDirectory   _ = Context.run .: Directory.copy

    uploadFile      _ = Context.run . File.upload "."
    fetchFile       _ = Context.run . File.fetch "."
    fileExists      _ = Context.run . File.exists
    removeFile      _ = Context.run . File.remove
    copyFile        _ = Context.run .: File.copy
    moveFile        _ = Context.run .: File.rename

    resolvePath     _ = return
    stat            _ = return . toGen -- TODO [PM] : implement me


