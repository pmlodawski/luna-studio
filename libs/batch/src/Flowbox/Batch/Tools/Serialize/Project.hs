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

import qualified Projects_Types                                           as TProject
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Batch.Project.Project                              (Project)
import qualified Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Projects   ()
import qualified Flowbox.Luna.Lib.LibManager                              as LibManager
import qualified Flowbox.System.IO.Serializer                             as Serializer
import           Flowbox.System.IO.Serializer                               (Serializable(..), Deserializable(..))
import           Flowbox.System.UniPath                                     (UniPath)
import           Flowbox.Tools.Conversion                                   


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
    let 
        ppath    = Project.path project
        sproject = Serializable ppath (saveProject project)

    Serializer.serialize sproject


restoreProject :: UniPath -> IO Project
restoreProject ppath = do
    let dproject = Deserializable ppath getProject

    Serializer.deserialize dproject

