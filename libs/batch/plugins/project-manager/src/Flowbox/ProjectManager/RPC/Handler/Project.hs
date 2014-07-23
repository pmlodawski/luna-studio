---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.RPC.Handler.Project where

import qualified Data.Sequence as Sequence

import qualified Flowbox.Batch.Handler.Project                            as BatchP
import           Flowbox.Batch.Project.Project                            (Project)
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project   ()
import           Flowbox.Bus.RPC.RPC                                      (RPC)
import qualified Flowbox.Luna.Lib.LibManager                              as LibManager
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                           (ContextRef)
import qualified Flowbox.ProjectManager.Context                           as Context
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Close.Request     as Close
import qualified Generated.Proto.ProjectManager.Project.Close.Update      as Close
import qualified Generated.Proto.ProjectManager.Project.Create.Request    as Create
import qualified Generated.Proto.ProjectManager.Project.Create.Update     as Create
import qualified Generated.Proto.ProjectManager.Project.List.Request      as List
import qualified Generated.Proto.ProjectManager.Project.List.Status       as List
import qualified Generated.Proto.ProjectManager.Project.Lookup.Request    as Lookup
import qualified Generated.Proto.ProjectManager.Project.Lookup.Status     as Lookup
import qualified Generated.Proto.ProjectManager.Project.Modify.Request    as Modify
import qualified Generated.Proto.ProjectManager.Project.Modify.Update     as Modify
import qualified Generated.Proto.ProjectManager.Project.Open.Request      as Open
import qualified Generated.Proto.ProjectManager.Project.Open.Update       as Open
import qualified Generated.Proto.ProjectManager.Project.Store.Request     as Store
import qualified Generated.Proto.ProjectManager.Project.Store.Status      as Store



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.RPC.Handler.Project"

------ public api -------------------------------------------------


list :: ContextRef -> List.Request -> RPC IO List.Status
list ctxRef request = do
    projects <- Context.run ctxRef BatchP.projects
    let tprojects       = map (\a -> encode a ^. _1) projects
        tprojectsVector = Sequence.fromList tprojects
    return $ List.Status request tprojectsVector


lookup :: ContextRef -> Lookup.Request -> RPC IO Lookup.Status
lookup ctxRef request@(Lookup.Request tprojectID) = do
    let projectID = decodeP tprojectID
    project <- Context.run ctxRef $ BatchP.projectByID projectID
    return $ Lookup.Status request $ encode (projectID, project) ^. _1


create :: ContextRef -> Create.Request -> RPC IO Create.Update
create ctxRef request@(Create.Request tname tpath tattributes) = do
    let name = decodeP tname
        path = decodeP tpath
        attributes = decodeP tattributes
    newProject <- Context.run ctxRef $ BatchP.createProject name path attributes
    return $ Create.Update request $ encode newProject ^. _1


open :: ContextRef -> Open.Request -> RPC IO Open.Update
open ctxRef request@(Open.Request tpath) = do
    let upath = decodeP tpath
    (projectID, project) <- Context.run ctxRef $ BatchP.openProject upath
    return $ Open.Update request $ encode (projectID, project) ^. _1


modify :: ContextRef -> Modify.Request -> RPC IO Modify.Update
modify ctxRef request@(Modify.Request tproject) = do
    projectWithID <- decodeE (tproject, LibManager.empty) :: RPC IO (Project.ID, Project)
    Context.run ctxRef $ BatchP.updateProject projectWithID
    return $ Modify.Update request


close :: ContextRef -> Close.Request -> RPC IO Close.Update
close ctxRef request@(Close.Request tprojectID) = do
    let projectID = decodeP tprojectID
    Context.run ctxRef $ BatchP.closeProject projectID
    return $ Close.Update request


store :: ContextRef -> Store.Request -> RPC IO Store.Status
store ctxRef request@(Store.Request tprojectID) = do
    let projectID = decodeP tprojectID
    Context.run ctxRef $ BatchP.storeProject projectID
    return $ Store.Status request
