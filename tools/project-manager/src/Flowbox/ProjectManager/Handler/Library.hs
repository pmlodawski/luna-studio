---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.Library where

import qualified Data.IORef as IORef

import qualified Flowbox.Batch.Handler.Library                                as BatchL
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Library        ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                               (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Library.Library                              as Gen
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Args   as Create
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Result as Create
import qualified Generated.Proto.ProjectManager.Project.Library.List.Args     as List
import qualified Generated.Proto.ProjectManager.Project.Library.List.Result   as List
import qualified Generated.Proto.ProjectManager.Project.Library.Load.Args     as Load
import qualified Generated.Proto.ProjectManager.Project.Library.Load.Result   as Load
import qualified Generated.Proto.ProjectManager.Project.Library.Lookup.Args   as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.Lookup.Result as Lookup
import qualified Generated.Proto.ProjectManager.Project.Library.Store.Args    as Store
import qualified Generated.Proto.ProjectManager.Project.Library.Store.Result  as Store
import qualified Generated.Proto.ProjectManager.Project.Library.Unload.Args   as Unload
import qualified Generated.Proto.ProjectManager.Project.Library.Unload.Result as Unload



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ProjectManager.Handlers.Library"

-------- public api -------------------------------------------------


shrinkLibrary :: Gen.Library -> Gen.Library
shrinkLibrary library = library { Gen.ast = Nothing, Gen.propertyMap = Nothing}


list :: ContextRef -> List.Args -> IO List.Result
list ctx (List.Args tprojectID) = do
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    libs <- BatchL.libraries projectID batch
    return $ List.Result $ fmap shrinkLibrary $ encodeList libs


lookup :: ContextRef -> Lookup.Args -> IO Lookup.Result
lookup ctx (Lookup.Args tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    library <- BatchL.libraryByID libID projectID batch
    return $ Lookup.Result $ shrinkLibrary $ encode (libID, library)


create :: ContextRef -> Create.Args -> IO Create.Result
create ctx (Create.Args tname tpath tprojectID) = do
    let projectID = decodeP tprojectID
        name      = decodeP tname
        path      = decodeP tpath
    batch <- IORef.readIORef ctx
    (newBatch, newLibrary) <-  BatchL.createLibrary name path projectID batch
    IORef.writeIORef ctx newBatch
    return $ Create.Result $ encode newLibrary


load :: ContextRef -> Load.Args -> IO Load.Result
load ctx (Load.Args tpath tprojectID) = do
    let path      = decodeP tpath
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    (newBatch, (newLibID, newLibrary)) <- BatchL.loadLibrary path projectID batch
    IORef.writeIORef ctx newBatch
    return $ Load.Result $ encode (newLibID, newLibrary)


unload :: ContextRef -> Unload.Args -> IO Unload.Result
unload ctx (Unload.Args tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    newBatch <- BatchL.unloadLibrary libID projectID batch
    IORef.writeIORef ctx newBatch
    return Unload.Result


store :: ContextRef -> Store.Args -> IO Store.Result
store ctx (Store.Args tlibID tprojectID) =  do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctx
    BatchL.storeLibrary libID projectID batch
    return Store.Result


