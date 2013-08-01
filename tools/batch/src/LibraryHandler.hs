---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module LibraryHandler (
    libraries,
    loadLibrary,
    unloadLibrary,
    libraryRootDef
) 
where
    
import           Control.Exception
import           Data.IORef
import qualified Data.Text.Lazy as Text
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)


import qualified Defs_Types
import qualified Libs_Types
import           Batch_Types (ArgumentException(..))

import qualified Luna.Core           as Core
import           Luna.Core             (Core)
import qualified Luna.Lib.LibManager as LibManager
import           Luna.Lib.Library      (Library(..))
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Libs ()



libraries :: IORef Core -> IO (Vector Libs_Types.Library)
libraries batchHandler = do 
    putStr "getting libraries...\t\t"
    core <- readIORef batchHandler
    let libManager =  Core.libManager core
        libs = LibManager.labNodes libManager
        tlibs = map encode libs
        tlibsVector = Vector.fromList tlibs
    putStrLn "success!"
    return tlibsVector


loadLibrary :: IORef Core -> Maybe Libs_Types.Library -> IO Libs_Types.Library
loadLibrary = libOperation (\ batchHandler (_, library) -> do
    core <- readIORef batchHandler
    let libManager = Core.libManager core
        [libID]      = LibManager.newNodes 1 libManager
        newTLibrary = encode (libID, library)
        newCore = core { Core.libManager = LibManager.insNode (libID, library) libManager }
    writeIORef batchHandler newCore
    putStrLn "success!"
    return newTLibrary)


unloadLibrary :: IORef Core -> Maybe Libs_Types.Library -> IO ()
unloadLibrary = libOperation (\ batchHandler (libID, _) -> do
    core <- readIORef batchHandler
    let libManager = Core.libManager core
        newCore = core { Core.libManager = LibManager.delNode libID libManager }
    writeIORef batchHandler newCore)


libraryRootDef :: IORef Core -> Maybe Libs_Types.Library -> IO Defs_Types.NodeDef
libraryRootDef = libOperation (\ batchHandler (libID, library) -> do
    core <- readIORef batchHandler
    putStr $ "libraryRootDef - NOT IMPLEMENTED"
    return undefined)


libOperation :: (IORef Core -> (Int, Library) -> a)
             ->  IORef Core -> Maybe Libs_Types.Library -> a
libOperation operation batchHandler tlibrary =  case tlibrary of 
    (Just tlib) -> do 
        case decode tlib :: Either String (Int, Library) of
            Right library -> operation batchHandler library
            Left  message -> throw $ ArgumentException $ Just $ Text.pack message
    Nothing     -> throw $ ArgumentException $ Just $ Text.pack "`library` argument is missing";
