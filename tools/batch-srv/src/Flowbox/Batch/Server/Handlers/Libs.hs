---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Libs (
    libraries,

    createLibrary,
    loadLibrary,
    unloadLibrary,
    storeLibrary,
    buildLibrary,
    libraryRootDef,
) 
where


import           Control.Error                                               
import           Data.Int                                                    
import           Data.IORef                                                  
import qualified Data.Vector                                               as Vector
import           Data.Vector                                                 (Vector)
import           Data.Text.Lazy                                              (Text)

import qualified Defs_Types                                                as TDefs
import           Flowbox.Batch.Server.Handlers.Common                        
import qualified Libs_Types                                                as TLibs
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Batch                                         (Batch(..))
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library(..))
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Libs         ()


------ public api -------------------------------------------------

libraries :: IORef Batch -> IO (Vector TLibs.Library)
libraries batchHandler = tRunScript $ do
    scriptIO $ putStrLn "called libraries"
    batch <- tryReadIORef batchHandler
    libs  <- tryRight $ Batch.libraries batch 
    let tlibs       = map (fst . encode) libs
        tlibsVector = Vector.fromList tlibs
    return tlibsVector


createLibrary :: IORef Batch -> Maybe TLibs.Library -> IO TLibs.Library
createLibrary batchHandler mtlibrary = tRunScript $ do
    scriptIO $ putStrLn "called createLibrary"
    tlibrary     <- mtlibrary <?> "`library` argument is missing" 
    (_, library) <- tryRight (decode (tlibrary, DefManager.empty) :: Either String (Library.ID, Library))
    batch <- tryReadIORef batchHandler
    let libName = Library.name library
        libPath = Library.path library
    (newBatch, newLibrary) <-tryRight $  Batch.createLibrary libName libPath batch
    tryWriteIORef batchHandler newBatch
    return $ fst $ (encode newLibrary :: (TLibs.Library, DefManager))


loadLibrary :: IORef Batch -> Maybe Text -> IO TLibs.Library
loadLibrary batchHandler mtpath = tRunScript $ do
    scriptIO $ putStrLn "called loadLibrary"
    upath  <- tryGetUniPath mtpath "path"
    batch <- tryReadIORef batchHandler
    r     <- scriptIO $ Batch.loadLibrary upath batch
    (newBatch, (newLibID, newLibrary)) <- tryRight r
    tryWriteIORef batchHandler newBatch
    return $ fst $ encode (newLibID, newLibrary)


unloadLibrary :: IORef Batch -> Maybe Int32 -> IO ()
unloadLibrary batchHandler mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called unloadLibrary"
    libID    <- tryGetID mtlibID "libID"
    batch    <- tryReadIORef batchHandler
    newBatch <- tryRight $ Batch.unloadLibrary libID batch 
    tryWriteIORef batchHandler newBatch


storeLibrary :: IORef Batch -> Maybe Int32 -> IO ()
storeLibrary batchHandler mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called storeLibrary"
    libID <- tryGetID mtlibID "libID"
    batch <- tryReadIORef batchHandler
    _ <- scriptIO $ Batch.storeLibrary libID batch
    return ()


buildLibrary :: IORef Batch -> Maybe Int32 -> IO ()
buildLibrary batchHandler mtlibID  = tRunScript $ do
    scriptIO $ putStrLn "called buildLibrary"
    libID <- tryGetID mtlibID "libID"
    batch <- tryReadIORef batchHandler
    _ <- scriptIO $ Batch.buildLibrary libID batch
    return ()


libraryRootDef :: IORef Batch -> Maybe Int32 -> IO TDefs.Definition
libraryRootDef batchHandler mtlibID  = tRunScript $ do
    scriptIO $ putStrLn "called libraryRootDef"
    libID <- tryGetID mtlibID "libID"
    batch <- tryReadIORef batchHandler
    (arootDefID, rootDef) <- tryRight $ Batch.libraryRootDef libID batch
    return $ fst $ encode (arootDefID, rootDef)
