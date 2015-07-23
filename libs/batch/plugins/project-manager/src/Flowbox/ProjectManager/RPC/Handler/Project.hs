---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.ProjectManager.RPC.Handler.Project where

import qualified Data.Sequence as Sequence

import qualified Flowbox.Batch.Batch                                   as Batch
import qualified Flowbox.Batch.Handler.Common                          as Batch
import qualified Flowbox.Batch.Handler.Project                         as BatchP
import           Flowbox.Batch.Project.Project                         (Project)
import qualified Flowbox.Batch.Project.Project                         as Project
import           Flowbox.Bus.Data.Message                              (Message)
import           Flowbox.Bus.Data.Topic                                (Topic)
import           Flowbox.Bus.RPC.RPC                                   (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                       hiding (Context)
import           Flowbox.ProjectManager.Context                        (Context)
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.Utils                                                             (makeMsgArr, prepareResponse, serialize)
import qualified Generated.Proto.Project.Project                       as Gen
import qualified Generated.Proto.ProjectManager.Project.Close.Request  as Close
import qualified Generated.Proto.ProjectManager.Project.Close.Update   as Close
import qualified Generated.Proto.ProjectManager.Project.Create.Request as Create
import qualified Generated.Proto.ProjectManager.Project.Create.Update  as Create
import qualified Generated.Proto.ProjectManager.Project.List.Request   as List
import qualified Generated.Proto.ProjectManager.Project.List.Status    as List
import qualified Generated.Proto.ProjectManager.Project.Lookup.Request as Lookup
import qualified Generated.Proto.ProjectManager.Project.Lookup.Status  as Lookup
import qualified Generated.Proto.ProjectManager.Project.Modify.Request as Modify
import qualified Generated.Proto.ProjectManager.Project.Modify.Update  as Modify
import qualified Generated.Proto.ProjectManager.Project.Open.Request   as Open
import qualified Generated.Proto.ProjectManager.Project.Open.Update    as Open
import qualified Generated.Proto.ProjectManager.Project.Store.Request  as Store
import qualified Generated.Proto.ProjectManager.Project.Store.Status   as Store
import qualified Generated.Proto.Urm.URM.ClearStack.Request            as ClearStack
import           Luna.DEP.Data.Serialize.Proto.Conversion.Attributes   ()



logger :: LoggerIO
logger = getLoggerIO $moduleName

------ public api -------------------------------------------------

encodeProject :: (Project.ID, Project) -> Gen.Project
encodeProject = encode


list :: List.Request -> RPC Context IO List.Status
list request = do
    projects <- BatchP.projects
    let tprojects       = map (encodeProject . set (_2 . Project.libs) def) projects
        tprojectsVector = Sequence.fromList tprojects
    return $ List.Status request tprojectsVector


lookup :: Lookup.Request -> RPC Context IO Lookup.Status
lookup request@(Lookup.Request tprojectID) = do
    let projectID = decodeP tprojectID
    project <- BatchP.projectByID projectID
    return $ Lookup.Status request $ (encodeProject (projectID, project & Project.libs .~ def))


create :: Create.Request -> RPC Context IO Create.Update
create request@(Create.Request tname tpath tattributes) = do
    let name = fmap decodeP tname
        path = decodeP tpath
        attributes = decodeP tattributes
    newProject <- BatchP.createProject name path attributes
    Create.Update request (encodeProject $ _2 . Project.libs .~ def $ newProject) <$> Batch.getUpdateNo


open :: Open.Request -> RPC Context IO Open.Update
open request@(Open.Request tpath) = do
    let upath = decodeP tpath
    (projectID, project) <- BatchP.openProject upath
    Open.Update request (encodeProject (projectID, project & Project.libs .~ def)) <$> Batch.getUpdateNo


modify :: Modify.Request -> RPC Context IO Modify.Update
modify request@(Modify.Request tproject) = do
    projectWithID <- decodeE tproject :: RPC Context IO (Project.ID, Project)
    BatchP.updateProject projectWithID
    Modify.Update request <$> Batch.getUpdateNo


close :: Close.Request -> Maybe Topic -> RPC Context IO ([Close.Update], [Message])
close request@(Close.Request tprojectID) clearStackTopic = do
    BatchP.closeProject $ decodeP tprojectID
    Batch.put =<< (Batch.idMap .~ Batch.emptyIDMap) <$> Batch.get
    flip (,) (makeMsgArr (ClearStack.Request tprojectID) clearStackTopic)
             . return . (Close.Update request) <$> Batch.getUpdateNo


store :: Store.Request -> RPC Context IO Store.Status
store request@(Store.Request tprojectID tlibIDs mtpath) = do
    BatchP.storeProject (decodeP tprojectID) (decodeP tlibIDs) $ fmap decodeP mtpath
    return $ Store.Status request
