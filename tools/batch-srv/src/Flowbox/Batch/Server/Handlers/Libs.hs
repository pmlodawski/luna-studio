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
    libraryRootDef
) 
where
    
import           Data.IORef                                                  
import qualified Data.Vector                                               as Vector
import           Data.Vector                                                 (Vector)

import qualified Defs_Types                                                as TDefs
import           Flowbox.Batch.Server.Handlers.Common                        
import qualified Libs_Types                                                as TLibs
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Batch                                         (Batch(..))
import           Flowbox.Luna.Lib.Library                                    (Library(..))
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Libs         ()



------ public api helpers -----------------------------------------
libOperation :: (IORef Batch -> (Int, Library) -> a)
             ->  IORef Batch -> Maybe TLibs.Library -> a
libOperation operation batchHandler tlibrary = case tlibrary of 
        (Just tlib) -> do 
            case decode tlib :: Either String (Int, Library) of
                Right library -> operation batchHandler library
                Left  message -> throw' message
        Nothing     -> throw' "`library` argument is missing";


------ public api -------------------------------------------------
libraries :: IORef Batch -> IO (Vector TLibs.Library)
libraries batchHandler = do 
    putStrLn "call libraries"
    batch <- readIORef batchHandler
    case Batch.libraries batch of
        Left message -> throw' message
        Right libs -> do 
            let tlibs       = map encode libs
                tlibsVector = Vector.fromList tlibs
            return tlibsVector


createLibrary :: IORef Batch -> Maybe TLibs.Library -> IO TLibs.Library
createLibrary = libOperation (\ _ (_, library) -> do
    putStrLn "call createLibrary - NOT YET IMPLEMENTED"
    return $ encode (-1, library))


loadLibrary :: IORef Batch -> Maybe TLibs.Library -> IO TLibs.Library
loadLibrary = libOperation (\ batchHandler (_, library) -> do
    putStrLn "call loadLibrary"
    batch <- readIORef batchHandler
    r     <- Batch.loadLibrary library batch
    case r of
        Left message                             -> throw' message
        Right (newBatch, (newLibID, newLibrary)) -> do
            let newTLibrary = encode (newLibID, newLibrary)
            writeIORef batchHandler newBatch
            return newTLibrary)


unloadLibrary :: IORef Batch -> Maybe TLibs.Library -> IO ()
unloadLibrary = libOperation (\ batchHandler (libID, _) -> do
    putStrLn "call unloadLibrary"
    batch <- readIORef batchHandler
    case Batch.unloadLibrary libID batch of 
        Left message   -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)


storeLibrary :: IORef Batch -> Maybe TLibs.Library -> IO ()
storeLibrary = libOperation (\ batchHandler (libID, _) -> do
    batch <- readIORef batchHandler
    r     <- Batch.storeLibrary libID batch
    case r of 
        Left message -> throw' message
        Right ()     -> return ())


libraryRootDef :: IORef Batch -> Maybe TLibs.Library -> IO TDefs.Definition
libraryRootDef = libOperation (\ batchHandler (_, library) -> do
    putStrLn "call libraryRootDef"
    batch <- readIORef batchHandler
    case Batch.libraryRootDef library batch of 
        Left message               -> throw' message
        Right (arootDefID, rootDef) -> do 
            let (trootDef, _) = encode (arootDefID, rootDef)
            return trootDef)
         