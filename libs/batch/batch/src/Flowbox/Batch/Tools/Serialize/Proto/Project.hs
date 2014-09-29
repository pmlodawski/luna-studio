---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Batch.Tools.Serialize.Proto.Project (
    storeProject,
    restoreProject,
) where

import qualified Data.ByteString.Lazy as ByteString
import           System.IO
import qualified Text.ProtocolBuffers as Proto

import           Flowbox.Batch.Project.Project                          (Project)
import qualified Flowbox.Batch.Project.Project                          as Project
import qualified Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project ()
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.IO.Serializer                           (Deserializable (..), Serializable (..))
import qualified Flowbox.System.IO.Serializer                           as Serializer
import           Flowbox.System.UniPath                                 (UniPath)
import qualified Flowbox.System.UniPath                                 as UniPath
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Project.Project                        as Gen
import qualified Luna.Lib.Manager                                       as LibManager



projectFile :: String
projectFile = "project.flowbox"


saveProject :: Project -> Handle -> IO ()
saveProject project h =
    ByteString.hPut h $ Proto.messagePut $ encode (-1::Project.ID, project) ^. _1


getProject :: Handle -> IO Project
getProject h = runScript $ do
    bytes                        <- scriptIO $ ByteString.hGetContents h
    (tproject :: Gen.Project, _) <- tryRight $ Proto.messageGet bytes
    (_ :: Project.ID, project)   <- tryRight $ decode (tproject, LibManager.empty)
    return project


storeProject :: Project -> IO ()
storeProject project = do
    let filepath = UniPath.append projectFile $ project ^. Project.path
        sproject = Serializable filepath (saveProject project)
    Serializer.serialize sproject


restoreProject :: UniPath -> IO Project
restoreProject upath = do
    let filepath = UniPath.append projectFile upath
        dproject = Deserializable filepath getProject
    project <- Serializer.deserialize dproject
    return $ project & Project.path .~ upath

