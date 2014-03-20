---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.Project where

import qualified Data.IORef    as IORef
import qualified Data.Sequence as Sequence

import qualified Flowbox.Batch.Handler.Project                            as BatchP
import qualified Flowbox.Batch.Process.Map                                as ProcessMap
import           Flowbox.Batch.Project.Project                            (Project)
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project   ()
import qualified Flowbox.Luna.Lib.LibManager                              as LibManager
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                           (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Close.Args        as Close
import qualified Generated.Proto.ProjectManager.Project.Close.Result      as Close
import qualified Generated.Proto.ProjectManager.Project.Create.Args       as Create
import qualified Generated.Proto.ProjectManager.Project.Create.Result     as Create
import qualified Generated.Proto.ProjectManager.Project.List.Args         as List
import qualified Generated.Proto.ProjectManager.Project.List.Result       as List
import qualified Generated.Proto.ProjectManager.Project.Lookup.Args       as Lookup
import qualified Generated.Proto.ProjectManager.Project.Lookup.Result     as Lookup
import qualified Generated.Proto.ProjectManager.Project.Open.Args         as Open
import qualified Generated.Proto.ProjectManager.Project.Open.Result       as Open
import qualified Generated.Proto.ProjectManager.Project.Store.Args        as Store
import qualified Generated.Proto.ProjectManager.Project.Store.Result      as Store
import qualified Generated.Proto.ProjectManager.Project.Update.Args       as Update
import qualified Generated.Proto.ProjectManager.Project.Update.Result     as Update



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ProjectManager.Handler.Project"

------ public api -------------------------------------------------


list :: ContextRef -> List.Args -> IO List.Result
list ctx _ = do
    batch <- IORef.readIORef ctx
    let aprojects       = BatchP.projects batch
        tprojects       = map (\a -> encode a ^. _1) aprojects
        tprojectsVector = Sequence.fromList tprojects
    return $ List.Result tprojectsVector


lookup :: ContextRef -> Lookup.Args -> IO Lookup.Result
lookup ctx (Lookup.Args tprojectID) = do
    let projectID = decodeP tprojectID
    batch     <- IORef.readIORef ctx
    project   <- BatchP.projectByID projectID batch
    return $ Lookup.Result $ encode (projectID, project) ^. _1


create :: ContextRef -> Create.Args -> IO Create.Result
create ctx (Create.Args tname tpath tattributes) = do
    let name = decodeP tname
        path = decodeP tpath
        attributes = decodeP tattributes
    batch <- IORef.readIORef ctx
    (newBatch, newProject) <- BatchP.createProject name path attributes batch
    IORef.writeIORef ctx newBatch
    return $ Create.Result $ encode newProject ^. _1


open :: ContextRef -> Open.Args -> IO Open.Result
open ctx (Open.Args tpath) = do
    let upath = decodeP tpath
    batch <- IORef.readIORef ctx
    (newBatch, (projectID, aproject)) <- BatchP.openProject upath batch
    IORef.writeIORef ctx newBatch
    return $ Open.Result $ encode (projectID, aproject) ^. _1


update :: ContextRef -> Update.Args -> IO Update.Result
update ctx  (Update.Args tproject) = do
    project <- (decode (tproject, LibManager.empty, ProcessMap.empty) :: IO (Project.ID, Project))
    batch   <- IORef.readIORef ctx
    newBatch <-  BatchP.updateProject project batch
    IORef.writeIORef ctx newBatch
    return Update.Result


close :: ContextRef -> Close.Args -> IO Close.Result
close ctx (Close.Args tprojectID) = do
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    let newBatch = BatchP.closeProject projectID batch
    IORef.writeIORef ctx newBatch
    return Close.Result


store :: ContextRef -> Store.Args -> IO Store.Result
store ctx (Store.Args tprojectID) = do
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    BatchP.storeProject projectID batch
    return Store.Result
