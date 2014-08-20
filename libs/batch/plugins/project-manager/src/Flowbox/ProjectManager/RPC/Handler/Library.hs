---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.RPC.Handler.Library where

import qualified Flowbox.Batch.Handler.Common                                  as Batch
import qualified Flowbox.Batch.Handler.Library                                 as BatchL
import           Flowbox.Bus.RPC.RPC                                           (RPC)
import           Flowbox.Prelude                                               hiding (Context)
import           Flowbox.ProjectManager.Context                                (Context)
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
import           Luna.Data.Serialize.Proto.Conversion.Library                  ()



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.RPC.Handler.Library"

-------- public api -------------------------------------------------


shrinkLibrary :: Gen.Library -> Gen.Library
shrinkLibrary library = library { Gen.ast = Nothing, Gen.propertyMap = Nothing}


list :: List.Request -> RPC Context IO List.Status
list request@(List.Request tprojectID) = do
    let projectID = decodeP tprojectID
    libs <- BatchL.libraries projectID
    return $ List.Status request (shrinkLibrary <$> encodeList libs)


lookup :: Lookup.Request -> RPC Context IO Lookup.Status
lookup request@(Lookup.Request tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    library <- BatchL.libraryByID libID projectID
    return $ Lookup.Status request (shrinkLibrary $ encode (libID, library))


create :: Create.Request -> RPC Context IO Create.Update
create request@(Create.Request tname tpath tprojectID) = do
    let projectID = decodeP tprojectID
        name      = decodeP tname
        path      = decodeP tpath
    newLibrary <- BatchL.createLibrary name path projectID
    updateNo <- Batch.getUpdateNo
    return $ Create.Update request (encode newLibrary) updateNo


load :: Load.Request -> RPC Context IO Load.Update
load request@(Load.Request tpath tprojectID) = do
    let path      = decodeP tpath
        projectID = decodeP tprojectID
    (newLibID, newLibrary) <- BatchL.loadLibrary path projectID
    updateNo <- Batch.getUpdateNo
    return $ Load.Update request (encode (newLibID, newLibrary)) updateNo


unload :: Unload.Request -> RPC Context IO Unload.Update
unload request@(Unload.Request tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchL.unloadLibrary libID projectID
    updateNo <- Batch.getUpdateNo
    return $ Unload.Update request updateNo


store :: Store.Request -> RPC Context IO Store.Status
store request@(Store.Request tlibID tprojectID) = do
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchL.storeLibrary libID projectID
    return $ Store.Status request
