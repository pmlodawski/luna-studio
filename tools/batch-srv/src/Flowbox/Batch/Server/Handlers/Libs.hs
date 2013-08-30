---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Libs (
    libraries,

    libraryByID,
    createLibrary,
    loadLibrary,
    unloadLibrary,
    storeLibrary,
    buildLibrary,
    libraryRootDef,
) 
where


import           Data.Int                                              (Int32)
import           Data.IORef                                            
import qualified Data.Vector                                         as Vector
import           Data.Vector                                           (Vector)
import           Data.Text.Lazy                                        (Text)

import qualified Defs_Types                                          as TDefs
import           Flowbox.Batch.Server.Handlers.Common                  (tRunScript)
import qualified Libs_Types                                          as TLibs
import           Flowbox.Batch.Batch                                   (Batch(..))
import qualified Flowbox.Batch.Handlers.Libs                         as BatchL
import           Flowbox.Control.Error                                 
import qualified Flowbox.Luna.Lib.Library                            as Library
import           Flowbox.Luna.Lib.Library                              (Library(..))
import qualified Flowbox.Luna.Network.Def.DefManager                 as DefManager
import           Flowbox.Luna.Network.Def.DefManager                   (DefManager)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs   ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Libs   ()
import           Flowbox.System.Log.Logger                             (getLogger, info)
import           Flowbox.Tools.Conversion                              



logger :: (String -> IO ()) -> IO ()
logger = getLogger "Flowbox.Batch.Server.Handlers.Libs"

------ public api -------------------------------------------------

libraries :: IORef Batch -> Maybe Int32 -> IO (Vector TLibs.Library)
libraries batchHandler mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called libraries"
    batch     <- tryReadIORef batchHandler
    projectID <- tryGetID mtprojectID "projectID"
    libs      <- tryRight $ BatchL.libraries projectID batch 
    let tlibs       = map (fst . encode) libs
        tlibsVector = Vector.fromList tlibs
    return tlibsVector


libraryByID :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO TLibs.Library
libraryByID batchHandler mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called libraryByID"
    libID     <- tryGetID mtlibID "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    library   <- tryRight $ BatchL.libraryByID libID projectID batch
    return $ fst $ encode (libID, library)


createLibrary :: IORef Batch -> Maybe TLibs.Library -> Maybe Int32 -> IO TLibs.Library
createLibrary batchHandler mtlibrary mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called createLibrary"
    tlibrary     <- mtlibrary <??> "'library' argument is missing" 
    (_, library) <- tryRight (decode (tlibrary, DefManager.empty) :: Either String (Library.ID, Library))
    projectID    <- tryGetID mtprojectID "projectID"
    batch        <- tryReadIORef batchHandler
    let libName = Library.name library
        libPath = Library.path library
    (newBatch, newLibrary) <-tryRight $  BatchL.createLibrary libName libPath projectID batch
    tryWriteIORef batchHandler newBatch
    return $ fst $ (encode newLibrary :: (TLibs.Library, DefManager))


loadLibrary :: IORef Batch -> Maybe Text -> Maybe Int32 -> IO TLibs.Library
loadLibrary batchHandler mtpath mtprojectID= tRunScript $ do
    scriptIO $ logger.info $ "called loadLibrary"
    upath     <- tryGetUniPath mtpath "path"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    (newBatch, (newLibID, newLibrary)) <- scriptIO $ BatchL.loadLibrary upath projectID batch
    tryWriteIORef batchHandler newBatch
    return $ fst $ encode (newLibID, newLibrary)


unloadLibrary :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO ()
unloadLibrary batchHandler mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called unloadLibrary"
    libID     <- tryGetID mtlibID "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    newBatch  <- tryRight $ BatchL.unloadLibrary libID projectID batch 
    tryWriteIORef batchHandler newBatch


storeLibrary :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO ()
storeLibrary batchHandler mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called storeLibrary"
    libID     <- tryGetID mtlibID "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ BatchL.storeLibrary libID projectID batch
    return ()


buildLibrary :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO ()
buildLibrary batchHandler mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called buildLibrary"
    libID     <- tryGetID mtlibID "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ BatchL.buildLibrary libID projectID batch
    return ()


libraryRootDef :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO TDefs.Definition
libraryRootDef batchHandler mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called libraryRootDef"
    libID     <- tryGetID mtlibID "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    (arootDefID, rootDef) <- tryRight $ BatchL.libraryRootDef libID projectID batch
    return $ fst $ encode (arootDefID, rootDef)
