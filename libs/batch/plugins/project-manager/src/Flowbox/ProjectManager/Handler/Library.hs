---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.Library where

import qualified Flowbox.Batch.Handler.Library                                 as BatchL
import           Flowbox.Bus.RPC.RPC                                           (RPC)
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Library         ()
import           Flowbox.Prelude
import           Flowbox.ProjectManager.Context                                (ContextRef)
import qualified Flowbox.ProjectManager.Context                                as Context
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



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Handlers.Library"

-------- public api -------------------------------------------------


shrinkLibrary :: Gen.Library -> Gen.Library
shrinkLibrary library = library { Gen.ast = Nothing, Gen.propertyMap = Nothing}


list :: ContextRef -> List.Request -> RPC List.Status
list ctxRef (List.Request tprojectID) = do
    let projectID = decodeP tprojectID
    libs <- Context.run ctxRef $ BatchL.libraries projectID
    return $ List.Status (shrinkLibrary <$> encodeList libs) tprojectID


lookup :: ContextRef -> Lookup.Request -> RPC Lookup.Status
lookup ctxRef (Lookup.Request tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    library <- Context.run ctxRef $ BatchL.libraryByID libID projectID
    return $ Lookup.Status (shrinkLibrary $ encode (libID, library)) tprojectID


create :: ContextRef -> Create.Request -> RPC Create.Update
create ctxRef (Create.Request tname tpath tprojectID) = do
    let projectID = decodeP tprojectID
        name      = decodeP tname
        path      = decodeP tpath
    newLibrary <- Context.run ctxRef $ BatchL.createLibrary name path projectID
    return $ Create.Update (encode newLibrary) tprojectID


load :: ContextRef -> Load.Request -> RPC Load.Update
load ctxRef (Load.Request tpath tprojectID) = do
    let path      = decodeP tpath
        projectID = decodeP tprojectID
    (newLibID, newLibrary) <- Context.run ctxRef $ BatchL.loadLibrary path projectID
    return $ Load.Update (encode (newLibID, newLibrary)) tprojectID


unload :: ContextRef -> Unload.Request -> RPC Unload.Update
unload ctxRef (Unload.Request tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchL.unloadLibrary libID projectID
    return $ Unload.Update tlibID tprojectID


store :: ContextRef -> Store.Request -> RPC Store.Status
store ctxRef (Store.Request tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchL.storeLibrary libID projectID
    return $ Store.Status tlibID tprojectID
