---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Batch.Tools.Serialize.Proto.Project (
    storeProject,
    restoreProject,
) where

import qualified Data.ByteString.Lazy                                   as ByteString
import           System.IO                                                
import qualified Text.ProtocolBuffers                                   as Proto

import           Flowbox.Prelude                                          
import qualified Flowbox.Batch.Project.Project                          as Project
import           Flowbox.Batch.Project.Project                            (Project)
import qualified Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project   ()
import qualified Flowbox.Luna.Lib.LibManager                            as LibManager
import qualified Flowbox.System.IO.Serializer                           as Serializer
import           Flowbox.System.IO.Serializer                             (Serializable(..), Deserializable(..))
import qualified Flowbox.System.UniPath                                 as UniPath
import           Flowbox.System.UniPath                                   (UniPath)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic           
import           Flowbox.Control.Error                                    
import qualified Generated.Proto.Project.Project                        as Gen



projectFile :: String
projectFile = "project.flowbox"


saveProject :: Project -> Handle -> IO ()
saveProject project h = 
    ByteString.hPut h $ Proto.messagePut $ fst $ encode (-1::Project.ID, project)


getProject :: Handle -> IO Project
getProject h = runScript $ do 
    bytes                        <- scriptIO $ ByteString.hGetContents h
    (tproject :: Gen.Project, _) <- tryRight $ Proto.messageGet bytes
    (_ :: Project.ID, project)   <- tryRight $ decode (tproject, LibManager.empty)
    return project


storeProject :: Project -> IO ()
storeProject project = do 
    let filepath = UniPath.append projectFile $ Project.path project
        sproject = Serializable filepath (saveProject project)
    Serializer.serialize sproject


restoreProject :: UniPath -> IO Project
restoreProject upath = do
    let filepath = UniPath.append projectFile upath
        dproject = Deserializable filepath getProject
    project <- Serializer.deserialize dproject
    return $ project{Project.path = upath}

