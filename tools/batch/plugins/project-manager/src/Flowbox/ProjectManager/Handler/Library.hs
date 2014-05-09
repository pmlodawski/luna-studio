---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.Library where

import qualified Data.IORef as IORef

import qualified Flowbox.Batch.Handler.Library                                 as BatchL
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Library         ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Library.Library                               as Gen
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Request as Create
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Update  as Create
import qualified Generated.Proto.ProjectManager.Project.Library.List.Request   as List
import qualified Generated.Proto.ProjectManager.Project.Library.List.Status    as List
import qualified Generated.Proto.ProjectManager.Project.Library.Load.Request   as Load
import qualified Generated.Proto.ProjectManager.Project.Library.Load.Update    as Load
import qualified Generated.Proto.ProjectManager.Project.Library.Lookup.Request as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.Lookup.Status  as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.Store.Request  as Store
import qualified Generated.Proto.ProjectManager.Project.Library.Store.Status   as Store
import qualified Generated.Proto.ProjectManager.Project.Library.Unload.Request as Unload
import qualified Generated.Proto.ProjectManager.Project.Library.Unload.Update  as Unload



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ProjectManager.Handlers.Library"

-------- public api -------------------------------------------------


shrinkLibrary :: Gen.Library -> Gen.Library
shrinkLibrary library = library { Gen.ast = Nothing, Gen.propertyMap = Nothing}


list :: ContextRef -> List.Request -> IO List.Status
list ctx (List.Request tprojectID) = do
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    libs <- BatchL.libraries projectID batch
    return $ List.Status (fmap shrinkLibrary $ encodeList libs) tprojectID


lookup :: ContextRef -> Lookup.Request -> IO Lookup.Status
lookup ctx (Lookup.Request tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    library <- BatchL.libraryByID libID projectID batch
    return $ Lookup.Status (shrinkLibrary $ encode (libID, library)) tprojectID


create :: ContextRef -> Create.Request -> IO Create.Update
create ctx (Create.Request tname tpath tprojectID) = do
    let projectID = decodeP tprojectID
        name      = decodeP tname
        path      = decodeP tpath
    batch <- IORef.readIORef ctx
    (newBatch, newLibrary) <-  BatchL.createLibrary name path projectID batch
    IORef.writeIORef ctx newBatch
    return $ Create.Update (encode newLibrary) tprojectID


load :: ContextRef -> Load.Request -> IO Load.Update
load ctx (Load.Request tpath tprojectID) = do
    let path      = decodeP tpath
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    (newBatch, (newLibID, newLibrary)) <- BatchL.loadLibrary path projectID batch
    IORef.writeIORef ctx newBatch
    return $ Load.Update (encode (newLibID, newLibrary)) tprojectID


unload :: ContextRef -> Unload.Request -> IO Unload.Update
unload ctx (Unload.Request tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    newBatch <- BatchL.unloadLibrary libID projectID batch
    IORef.writeIORef ctx newBatch
    return $ Unload.Update tlibID tprojectID


store :: ContextRef -> Store.Request -> IO Store.Status
store ctx (Store.Request tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    BatchL.storeLibrary libID projectID batch
    return $ Store.Status tlibID tprojectID


