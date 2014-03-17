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
import           Flowbox.Prelude                                          hiding (Context)
import           Flowbox.ProjectManager.Context                           (Context)
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


list :: Context -> List.Args -> IO List.Result
list context _ = do
    batch <- IORef.readIORef context
    let aprojects       = BatchP.projects batch
        tprojects       = map (\a -> encode a ^. _1) aprojects
        tprojectsVector = Sequence.fromList tprojects
    return $ List.Result tprojectsVector


lookup :: Context -> Lookup.Args -> IO Lookup.Result
lookup context (Lookup.Args tprojectID) = do
    let projectID = decodeP tprojectID
    batch     <- IORef.readIORef context
    project   <- BatchP.projectByID projectID batch
    return $ Lookup.Result $ encode (projectID, project) ^. _1


create :: Context -> Create.Args -> IO Create.Result
create context (Create.Args tname tpath tattributes) = do
    let name = decodeP tname
        path = decodeP tpath
        attributes = decodeP tattributes
    batch <- IORef.readIORef context
    (newBatch, newProject) <- BatchP.createProject name path attributes batch
    IORef.writeIORef context newBatch
    return $ Create.Result $ encode newProject ^. _1


open :: Context -> Open.Args -> IO Open.Result
open context (Open.Args tpath) = do
    let upath = decodeP tpath
    batch <- IORef.readIORef context
    (newBatch, (projectID, aproject)) <- BatchP.openProject upath batch
    IORef.writeIORef context newBatch
    return $ Open.Result $ encode (projectID, aproject) ^. _1


update :: Context -> Update.Args -> IO Update.Result
update context  (Update.Args tproject) = do
    project <- (decode (tproject, LibManager.empty, ProcessMap.empty) :: IO (Project.ID, Project))
    batch   <- IORef.readIORef context
    newBatch <-  BatchP.updateProject project batch
    IORef.writeIORef context newBatch
    return Update.Result


close :: Context -> Close.Args -> IO Close.Result
close context (Close.Args tprojectID) = do
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef context
    let newBatch = BatchP.closeProject projectID batch
    IORef.writeIORef context newBatch
    return Close.Result


store :: Context -> Store.Args -> IO Store.Result
store context (Store.Args tprojectID) = do
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef context
    BatchP.storeProject projectID batch
    return Store.Result
