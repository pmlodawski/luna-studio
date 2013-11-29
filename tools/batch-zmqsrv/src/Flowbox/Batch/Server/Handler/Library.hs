---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.Library (
    libraries,
    libraryByID,
    createLibrary,
    loadLibrary,
    unloadLibrary,
    storeLibrary,
    buildLibrary,
    runLibrary,
) where

import           Data.IORef                                              (IORef)

import           Flowbox.Prelude                                         
import           Flowbox.Batch.Batch                                     (Batch)
import qualified Flowbox.Batch.Handler.Library                         as BatchL
import           Flowbox.Control.Error                                   
import           Flowbox.System.Log.Logger                               
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Library   ()
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic          
import qualified Generated.Proto.Batch.Library.Libraries.Args          as Libraries
import qualified Generated.Proto.Batch.Library.Libraries.Result        as Libraries
import qualified Generated.Proto.Batch.Library.LibraryByID.Args        as LibraryByID
import qualified Generated.Proto.Batch.Library.LibraryByID.Result      as LibraryByID
import qualified Generated.Proto.Batch.Library.CreateLibrary.Args      as CreateLibrary
import qualified Generated.Proto.Batch.Library.CreateLibrary.Result    as CreateLibrary
import qualified Generated.Proto.Batch.Library.LoadLibrary.Args        as LoadLibrary
import qualified Generated.Proto.Batch.Library.LoadLibrary.Result      as LoadLibrary
import qualified Generated.Proto.Batch.Library.UnloadLibrary.Args      as UnloadLibrary
import qualified Generated.Proto.Batch.Library.UnloadLibrary.Result    as UnloadLibrary
import qualified Generated.Proto.Batch.Library.StoreLibrary.Args       as StoreLibrary
import qualified Generated.Proto.Batch.Library.StoreLibrary.Result     as StoreLibrary
import qualified Generated.Proto.Batch.Library.BuildLibrary.Args       as BuildLibrary
import qualified Generated.Proto.Batch.Library.BuildLibrary.Result     as BuildLibrary
import qualified Generated.Proto.Batch.Library.RunLibrary.Args         as RunLibrary
import qualified Generated.Proto.Batch.Library.RunLibrary.Result       as RunLibrary



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Library"

-------- public api -------------------------------------------------

libraries :: IORef Batch -> Libraries.Args -> Script Libraries.Result
libraries batchHandler (Libraries.Args tprojectID) = do
    scriptIO $ loggerIO info "called libraries"
    let projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "projectID: " ++ (show projectID)
    libs <- tryRight $ BatchL.libraries projectID batch 
    return $ Libraries.Result $ encodeList libs


libraryByID :: IORef Batch -> LibraryByID.Args -> Script LibraryByID.Result
libraryByID batchHandler (LibraryByID.Args tlibID tprojectID) = do
    scriptIO $ loggerIO info "called libraryByID"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch     <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    library   <- tryRight $ BatchL.libraryByID libID projectID batch
    return $ LibraryByID.Result $ encode (libID, library)


createLibrary :: IORef Batch -> CreateLibrary.Args -> Script CreateLibrary.Result
createLibrary batchHandler (CreateLibrary.Args tname tpath tprojectID) = do
    scriptIO $ loggerIO info "called createLibrary"
    let projectID = decodeP tprojectID
        name      = decodeP tname
        path      = decodeP tpath
    batch        <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "name: " ++ (show name) ++ " path: " ++ (show path) ++ " projectID: " ++ (show projectID)
    (newBatch, newLibrary) <- tryRight $  BatchL.createLibrary name path projectID batch
    tryWriteIORef batchHandler newBatch
    return $ CreateLibrary.Result $ encode newLibrary


loadLibrary :: IORef Batch -> LoadLibrary.Args -> Script LoadLibrary.Result
loadLibrary batchHandler (LoadLibrary.Args tpath tprojectID) = do
    scriptIO $ loggerIO info "called loadLibrary"
    let path      = decodeP tpath
        projectID = decodeP tprojectID
    batch     <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "path: " ++ (show path) ++ " projectID: " ++ (show projectID)
    (newBatch, (newLibID, newLibrary)) <- scriptIO $ BatchL.loadLibrary path projectID batch
    tryWriteIORef batchHandler newBatch
    return $ LoadLibrary.Result $ encode (newLibID, newLibrary)


unloadLibrary :: IORef Batch -> UnloadLibrary.Args -> Script UnloadLibrary.Result
unloadLibrary batchHandler (UnloadLibrary.Args tlibID tprojectID) = do
    scriptIO $ loggerIO info "called unloadLibrary"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch     <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    newBatch  <- tryRight $ BatchL.unloadLibrary libID projectID batch 
    tryWriteIORef batchHandler newBatch
    return UnloadLibrary.Result


storeLibrary :: IORef Batch -> StoreLibrary.Args -> Script StoreLibrary.Result
storeLibrary batchHandler (StoreLibrary.Args tlibID tprojectID) =  do
    scriptIO $ loggerIO info "called storeLibrary"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch     <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    scriptIO $ BatchL.storeLibrary libID projectID batch
    return StoreLibrary.Result


buildLibrary :: IORef Batch -> BuildLibrary.Args -> Script BuildLibrary.Result
buildLibrary batchHandler (BuildLibrary.Args tlibID tprojectID) = do
    scriptIO $ loggerIO info "called buildLibrary"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch     <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    scriptIO $ BatchL.buildLibrary libID projectID batch
    return BuildLibrary.Result


runLibrary :: IORef Batch -> RunLibrary.Args -> Script RunLibrary.Result
runLibrary batchHandler (RunLibrary.Args tlibID tprojectID) = do
    scriptIO $ loggerIO info "called runLibrary"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch     <- tryReadIORef batchHandler
    scriptIO $ loggerIO debug $ "libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    output <- scriptIO $ BatchL.runLibrary libID projectID batch
    return $ RunLibrary.Result $ encodeP output


--libraryRootDef :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO TDefs.Definition
--libraryRootDef batchHandler mtlibID mtprojectID = tRunScript $ do
--    scriptIO $ loggerIO info "called libraryRootDef"
--    libID     <- tryGetID mtlibID "libID"
--    projectID <- tryGetID mtprojectID "projectID"
--    batch     <- tryReadIORef batchHandler
--    scriptIO $ loggerIO debug $ "libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
--    (arootDefID, rootDef) <- tryRight $ BatchL.libraryRootDef libID projectID batch
--    return $ fst $ encode (arootDefID, rootDef)
