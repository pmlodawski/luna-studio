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

import qualified Luna.Core                   as Core
import           Luna.Core                     (Core(..))
import qualified Luna.Lib.LibManager         as LibManager
import qualified Luna.Lib.Library            as Library
import           Luna.Lib.Library              (Library(..))
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Defs ()
import           Luna.Tools.Serialization.Libs ()



------ public api helpers -----------------------------------------
libOperation :: (IORef Core -> (Int, Library) -> a)
             ->  IORef Core -> Maybe Libs_Types.Library -> a
libOperation operation batchHandler tlibrary = case tlibrary of 
        (Just tlib) -> do 
            case decode tlib :: Either String (Int, Library) of
                Right library -> operation batchHandler library
                Left  message -> throw $ ArgumentException $ Just $ Text.pack message
        Nothing     -> throw $ ArgumentException $ Just $ Text.pack "`library` argument is missing";


------ public api -------------------------------------------------
libraries :: IORef Core -> IO (Vector Libs_Types.Library)
libraries batchHandler = do 
    putStrLn "call libraries"
    core <- readIORef batchHandler
    let libManager' =  Core.libManager core
        libs        = LibManager.labNodes libManager'
        tlibs       = map encode libs
        tlibsVector = Vector.fromList tlibs
    return tlibsVector


loadLibrary :: IORef Core -> Maybe Libs_Types.Library -> IO Libs_Types.Library
loadLibrary = libOperation (\ batchHandler (_, library) -> do
    putStrLn "call loadLibrary"
    core <- readIORef batchHandler
    let (newCore, newLibrary, newLibID) = Core.loadLibrary core library
        newTLibrary = encode (newLibID, newLibrary)
    writeIORef batchHandler newCore
    return newTLibrary)


unloadLibrary :: IORef Core -> Maybe Libs_Types.Library -> IO ()
unloadLibrary = libOperation (\ batchHandler (libID, _) -> do
    putStrLn "call unloadLibrary"
    core <- readIORef batchHandler
    let newCore = Core.unloadLibrary core libID
    writeIORef batchHandler newCore)


libraryRootDef :: IORef Core -> Maybe Libs_Types.Library -> IO Defs_Types.NodeDef
libraryRootDef = libOperation (\ batchHandler (_, library) -> do
    putStrLn "call libraryRootDef"
    core <- readIORef batchHandler
    let rootNodeDefID' = Library.rootNodeDefID library
        rootDef = Core.nodeDefByID core rootNodeDefID'
        (trootDef, _) = encode (rootNodeDefID', rootDef)
    return trootDef)

