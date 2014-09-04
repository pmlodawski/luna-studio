---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.RPC.Handler.Project where

import qualified Data.Sequence as Sequence

import qualified Flowbox.Batch.Handler.Common                           as Batch
import qualified Flowbox.Batch.Handler.Project                          as BatchP
import           Flowbox.Batch.Project.Project                          (Project)
import qualified Flowbox.Batch.Project.Project                          as Project
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project ()
import           Flowbox.Bus.RPC.RPC                                    (RPC)
import           Flowbox.Prelude                                        hiding (Context)
import           Flowbox.ProjectManager.Context                         (Context)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Close.Request   as Close
import qualified Generated.Proto.ProjectManager.Project.Close.Update    as Close
import qualified Generated.Proto.ProjectManager.Project.Create.Request  as Create
import qualified Generated.Proto.ProjectManager.Project.Create.Update   as Create
import qualified Generated.Proto.ProjectManager.Project.List.Request    as List
import qualified Generated.Proto.ProjectManager.Project.List.Status     as List
import qualified Generated.Proto.ProjectManager.Project.Lookup.Request  as Lookup
import qualified Generated.Proto.ProjectManager.Project.Lookup.Status   as Lookup
import qualified Generated.Proto.ProjectManager.Project.Modify.Request  as Modify
import qualified Generated.Proto.ProjectManager.Project.Modify.Update   as Modify
import qualified Generated.Proto.ProjectManager.Project.Open.Request    as Open
import qualified Generated.Proto.ProjectManager.Project.Open.Update     as Open
import qualified Generated.Proto.ProjectManager.Project.Store.Request   as Store
import qualified Generated.Proto.ProjectManager.Project.Store.Status    as Store
import           Luna.Data.Serialize.Proto.Conversion.Attributes        ()
import qualified Luna.Lib.Manager                                       as LibManager



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.RPC.Handler.Project"

------ public api -------------------------------------------------

list :: List.Request -> RPC Context IO List.Status
list request = do
    projects <- BatchP.projects
    let tprojects       = map (\a -> encode a ^. _1) projects
        tprojectsVector = Sequence.fromList tprojects
    return $ List.Status request tprojectsVector


lookup :: Lookup.Request -> RPC Context IO Lookup.Status
lookup request@(Lookup.Request tprojectID) = do
    let projectID = decodeP tprojectID
    project <- BatchP.projectByID projectID
    return $ Lookup.Status request $ encode (projectID, project) ^. _1


create :: Create.Request -> RPC Context IO Create.Update
create request@(Create.Request tname tpath tattributes) = do
    let name = decodeP tname
        path = decodeP tpath
        attributes = decodeP tattributes
    newProject <- BatchP.createProject name path attributes
    Create.Update request (encode newProject ^. _1) <$> Batch.getUpdateNo


open :: Open.Request -> RPC Context IO Open.Update
open request@(Open.Request tpath) = do
    let upath = decodeP tpath
    (projectID, project) <- BatchP.openProject upath
    Open.Update request (encode (projectID, project) ^. _1) <$> Batch.getUpdateNo


modify :: Modify.Request -> RPC Context IO Modify.Update
modify request@(Modify.Request tproject) = do
    projectWithID <- decodeE (tproject, LibManager.empty) :: RPC Context IO (Project.ID, Project)
    BatchP.updateProject projectWithID
    Modify.Update request <$> Batch.getUpdateNo


close :: Close.Request -> RPC Context IO Close.Update
close request@(Close.Request tprojectID) = do
    BatchP.closeProject $ decodeP tprojectID
    Close.Update request <$> Batch.getUpdateNo


store :: Store.Request -> RPC Context IO Store.Status
store request@(Store.Request tprojectID) = do
    BatchP.storeProject $ decodeP tprojectID
    return $ Store.Status request
