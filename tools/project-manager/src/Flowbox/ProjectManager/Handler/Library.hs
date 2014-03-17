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
import           Flowbox.Prelude                                              hiding (Context)
import           Flowbox.ProjectManager.Context                               (Context)
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


list :: Context -> List.Args -> IO List.Result
list context (List.Args tprojectID) = do
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef context
    libs <- BatchL.libraries projectID batch
    return $ List.Result $ fmap shrinkLibrary $ encodeList libs


lookup :: Context -> Lookup.Args -> IO Lookup.Result
lookup context (Lookup.Args tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef context
    library <- BatchL.libraryByID libID projectID batch
    return $ Lookup.Result $ shrinkLibrary $ encode (libID, library)


create :: Context -> Create.Args -> IO Create.Result
create context (Create.Args tname tpath tprojectID) = do
    let projectID = decodeP tprojectID
        name      = decodeP tname
        path      = decodeP tpath
    batch <- IORef.readIORef context
    (newBatch, newLibrary) <-  BatchL.createLibrary name path projectID batch
    IORef.writeIORef context newBatch
    return $ Create.Result $ encode newLibrary


load :: Context -> Load.Args -> IO Load.Result
load context (Load.Args tpath tprojectID) = do
    let path      = decodeP tpath
        projectID = decodeP tprojectID
    batch <- IORef.readIORef context
    (newBatch, (newLibID, newLibrary)) <- BatchL.loadLibrary path projectID batch
    IORef.writeIORef context newBatch
    return $ Load.Result $ encode (newLibID, newLibrary)


unload :: Context -> Unload.Args -> IO Unload.Result
unload context (Unload.Args tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef context
    newBatch <- BatchL.unloadLibrary libID projectID batch
    IORef.writeIORef context newBatch
    return Unload.Result


store :: Context -> Store.Args -> IO Store.Result
store context (Store.Args tlibID tprojectID) =  do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef context
    BatchL.storeLibrary libID projectID batch
    return Store.Result


