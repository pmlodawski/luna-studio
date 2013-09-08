---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Tools.Serialize.Project(
    storeProject,
    restoreProject,
) where

import           System.IO                                                  

import           Thrift.Protocol.Binary                                     
import           Thrift.Transport.Handle                                    ()

import           Flowbox.Prelude                           
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Batch.Project.Project                              (Project)
import qualified Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Projects   ()
import qualified Flowbox.Luna.Lib.LibManager                              as LibManager
import qualified Flowbox.System.IO.Serializer                             as Serializer
import           Flowbox.System.IO.Serializer                               (Serializable(..), Deserializable(..))
import qualified Flowbox.System.UniPath                                   as UniPath
import           Flowbox.System.UniPath                                     (UniPath)
import           Flowbox.Tools.Conversion                                   
import qualified Projects_Types                                           as TProject



projectFile :: String
projectFile = "project.flowbox"


saveProject :: Project -> Handle -> IO ()
saveProject project h = do 
    let (tproject, _) = encode (-1::Project.ID, project)
        protocol = BinaryProtocol h

    TProject.write_Project protocol tproject


getProject :: Handle -> IO Project
getProject h = do 
    let protocol = BinaryProtocol h
    tproject    <- TProject.read_Project protocol
    case decode (tproject, LibManager.empty) of 
        Left m             -> error m
        Right (_, project) -> return project


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

