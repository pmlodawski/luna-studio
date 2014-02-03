---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.Library where

import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           Flowbox.Batch.Batch                                   (Batch)
import qualified Flowbox.Batch.Handler.Library                         as BatchL
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Library ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.Library.BuildLibrary.Args       as BuildLibrary
import qualified Generated.Proto.Batch.Library.BuildLibrary.Result     as BuildLibrary
import qualified Generated.Proto.Batch.Library.CreateLibrary.Args      as CreateLibrary
import qualified Generated.Proto.Batch.Library.CreateLibrary.Result    as CreateLibrary
import qualified Generated.Proto.Batch.Library.InterpretLibrary.Args   as InterpretLibrary
import qualified Generated.Proto.Batch.Library.InterpretLibrary.Result as InterpretLibrary
import qualified Generated.Proto.Batch.Library.Libraries.Args          as Libraries
import qualified Generated.Proto.Batch.Library.Libraries.Result        as Libraries
import qualified Generated.Proto.Batch.Library.LibraryByID.Args        as LibraryByID
import qualified Generated.Proto.Batch.Library.LibraryByID.Result      as LibraryByID
import qualified Generated.Proto.Batch.Library.LoadLibrary.Args        as LoadLibrary
import qualified Generated.Proto.Batch.Library.LoadLibrary.Result      as LoadLibrary
import qualified Generated.Proto.Batch.Library.RunLibrary.Args         as RunLibrary
import qualified Generated.Proto.Batch.Library.RunLibrary.Result       as RunLibrary
import qualified Generated.Proto.Batch.Library.StoreLibrary.Args       as StoreLibrary
import qualified Generated.Proto.Batch.Library.StoreLibrary.Result     as StoreLibrary
import qualified Generated.Proto.Batch.Library.UnloadLibrary.Args      as UnloadLibrary
import qualified Generated.Proto.Batch.Library.UnloadLibrary.Result    as UnloadLibrary
import qualified Generated.Proto.Library.Library                       as Gen


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Library"

-------- public api -------------------------------------------------


shrinkLibrary :: Gen.Library -> Gen.Library
shrinkLibrary library = library { Gen.ast = Nothing, Gen.propertyMap = Nothing}


libraries :: IORef Batch -> Libraries.Args -> IO Libraries.Result
libraries batchHandler (Libraries.Args tprojectID) = do
    loggerIO info "called libraries"
    let projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    libs <- BatchL.libraries projectID batch
    return $ Libraries.Result $ fmap shrinkLibrary $ encodeList libs


libraryByID :: IORef Batch -> LibraryByID.Args -> IO LibraryByID.Result
libraryByID batchHandler (LibraryByID.Args tlibID tprojectID) = do
    loggerIO info "called libraryByID"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    library <- BatchL.libraryByID libID projectID batch
    return $ LibraryByID.Result $ shrinkLibrary $ encode (libID, library)


createLibrary :: IORef Batch -> CreateLibrary.Args -> IO CreateLibrary.Result
createLibrary batchHandler (CreateLibrary.Args tname tpath tprojectID) = do
    loggerIO info "called createLibrary"
    let projectID = decodeP tprojectID
        name      = decodeP tname
        path      = decodeP tpath
    batch <- IORef.readIORef batchHandler
    (newBatch, newLibrary) <-  BatchL.createLibrary name path projectID batch
    IORef.writeIORef batchHandler newBatch
    return $ CreateLibrary.Result $ encode newLibrary


loadLibrary :: IORef Batch -> LoadLibrary.Args -> IO LoadLibrary.Result
loadLibrary batchHandler (LoadLibrary.Args tpath tprojectID) = do
    loggerIO info "called loadLibrary"
    let path      = decodeP tpath
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    (newBatch, (newLibID, newLibrary)) <- BatchL.loadLibrary path projectID batch
    IORef.writeIORef batchHandler newBatch
    return $ LoadLibrary.Result $ encode (newLibID, newLibrary)


unloadLibrary :: IORef Batch -> UnloadLibrary.Args -> IO UnloadLibrary.Result
unloadLibrary batchHandler (UnloadLibrary.Args tlibID tprojectID) = do
    loggerIO info "called unloadLibrary"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    newBatch <- BatchL.unloadLibrary libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return UnloadLibrary.Result


storeLibrary :: IORef Batch -> StoreLibrary.Args -> IO StoreLibrary.Result
storeLibrary batchHandler (StoreLibrary.Args tlibID tprojectID) =  do
    loggerIO info "called storeLibrary"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    BatchL.storeLibrary libID projectID batch
    return StoreLibrary.Result


buildLibrary :: IORef Batch -> BuildLibrary.Args -> IO BuildLibrary.Result
buildLibrary batchHandler (BuildLibrary.Args tlibID tprojectID) = do
    loggerIO info "called buildLibrary"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    BatchL.buildLibrary libID projectID batch
    return BuildLibrary.Result


runLibrary :: IORef Batch -> RunLibrary.Args -> IO RunLibrary.Result
runLibrary batchHandler (RunLibrary.Args tlibID tprojectID) = do
    loggerIO info "called runLibrary"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    (newBatch, processID) <- BatchL.runLibrary libID projectID batch
    IORef.writeIORef batchHandler newBatch
    return $ RunLibrary.Result $ encodeP processID


interpretLibrary :: IORef Batch -> InterpretLibrary.Args -> IO InterpretLibrary.Result
interpretLibrary batchHandler (InterpretLibrary.Args tlibID tprojectID) = do
    loggerIO info "called interpretLibrary"
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef batchHandler
    BatchL.interpretLibrary libID projectID batch
    return InterpretLibrary.Result
