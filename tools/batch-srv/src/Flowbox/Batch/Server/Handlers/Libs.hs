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
    libraryRootDef,
    ------------
    libOperation,
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
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library(..))
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Libs         ()


------ public api helpers -----------------------------------------
libOperation :: ((Int, Library) -> a)
             -> Maybe TLibs.Library -> a
libOperation operation mtlibrary = case mtlibrary of 
        Nothing               -> throw' "`library` argument is missing";
        Just tlibrary         -> case decode (tlibrary, DefManager.empty) of
                Left  message -> throw' message
                Right library -> operation library


------ public api -------------------------------------------------
libraries :: IORef Batch -> IO (Vector TLibs.Library)
libraries batchHandler = do 
    putStrLn "call libraries"
    batch <- readIORef batchHandler
    case Batch.libraries batch of
        Left message -> throw' message
        Right libs -> do 
            let tlibs       = map (fst . encode) libs
                tlibsVector = Vector.fromList tlibs
            return tlibsVector


createLibrary :: IORef Batch -> Maybe TLibs.Library -> IO TLibs.Library
createLibrary batchHandler = libOperation (\ (_, library) -> do
    putStrLn "call createLibrary"
    batch <- readIORef batchHandler
    let libName = Library.name library
        libPath = Library.path library
    case Batch.createLibrary libName libPath batch of
        Left message -> throw' message
        Right (newBatch, newLibrary) -> 
            return $ fst $ (encode newLibrary :: (TLibs.Library, DefManager)))


loadLibrary :: IORef Batch -> Maybe TLibs.Library -> IO TLibs.Library
loadLibrary batchHandler = libOperation (\ (_, library) -> do
    putStrLn "call loadLibrary"
    batch <- readIORef batchHandler
    r     <- Batch.loadLibrary library batch
    case r of
        Left message                             -> throw' message
        Right (newBatch, (newLibID, newLibrary)) -> do
            let newTLibrary = fst $ encode (newLibID, newLibrary)
            writeIORef batchHandler newBatch
            return newTLibrary)


unloadLibrary :: IORef Batch -> Maybe TLibs.Library -> IO ()
unloadLibrary batchHandler = libOperation (\ (libID, _) -> do
    putStrLn "call unloadLibrary"
    batch <- readIORef batchHandler
    case Batch.unloadLibrary libID batch of 
        Left message   -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)


storeLibrary :: IORef Batch -> Maybe TLibs.Library -> IO ()
storeLibrary batchHandler = libOperation (\ (libID, _) -> do
    batch <- readIORef batchHandler
    r     <- Batch.storeLibrary libID batch
    case r of 
        Left message -> throw' message
        Right ()     -> return ())


libraryRootDef :: IORef Batch -> Maybe TLibs.Library -> IO TDefs.Definition
libraryRootDef batchHandler = libOperation (\ (libID, _) -> do
    putStrLn "call libraryRootDef"
    batch <- readIORef batchHandler
    case Batch.libraryRootDef libID batch of 
        Left message               -> throw' message
        Right (arootDefID, rootDef) -> do 
            let (trootDef, _) = encode (arootDefID, rootDef)
            return trootDef)
         