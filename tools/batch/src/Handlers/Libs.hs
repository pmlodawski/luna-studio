---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Handlers.Libs (
    libraries,
    createLibrary,
    loadLibrary,
    unloadLibrary,
    storeLibrary,
    libraryRootDef
) 
where
    
import           Data.IORef
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)


import qualified Defs_Types                  as TDefs
import           Handlers.Common
import qualified Libs_Types                  as TLibs
import qualified Luna.Core                   as Core
import           Luna.Core                     (Core(..))
import qualified Luna.Lib.LibManager         as LibManager
import qualified Luna.Lib.Library            as Library
import           Luna.Lib.Library              (Library(..))
import           Luna.Tools.Conversion
import           Luna.Tools.Conversion.Defs ()
import           Luna.Tools.Conversion.Libs ()



------ public api helpers -----------------------------------------
libOperation :: (IORef Core -> (Int, Library) -> a)
             ->  IORef Core -> Maybe TLibs.Library -> a
libOperation operation batchHandler tlibrary = case tlibrary of 
        (Just tlib) -> do 
            case decode tlib :: Either String (Int, Library) of
                Right library -> operation batchHandler library
                Left  message -> throw' message
        Nothing     -> throw' "`library` argument is missing";


------ public api -------------------------------------------------
libraries :: IORef Core -> IO (Vector TLibs.Library)
libraries batchHandler = do 
    putStrLn "call libraries"
    core <- readIORef batchHandler
    let libManager' =  Core.libManager core
        libs        = LibManager.labNodes libManager'
        tlibs       = map encode libs
        tlibsVector = Vector.fromList tlibs
    return tlibsVector


createLibrary :: IORef Core -> Maybe TLibs.Library -> IO TLibs.Library
createLibrary = libOperation (\ batchHandler (_, library) -> do
    putStrLn "call createLibrary - NOT YET IMPLEMENTED"
    return $ encode (-1, library))


loadLibrary :: IORef Core -> Maybe TLibs.Library -> IO TLibs.Library
loadLibrary = libOperation (\ batchHandler (_, library) -> do
    putStrLn "call loadLibrary"
    core <- readIORef batchHandler
    let (newCore, newLibrary, newLibID) = Core.loadLibrary core library
        newTLibrary = encode (newLibID, newLibrary)
    writeIORef batchHandler newCore
    return newTLibrary)


unloadLibrary :: IORef Core -> Maybe TLibs.Library -> IO ()
unloadLibrary = libOperation (\ batchHandler (libID, _) -> do
    putStrLn "call unloadLibrary"
    core <- readIORef batchHandler
    let newCore = Core.unloadLibrary core libID
    writeIORef batchHandler newCore)


storeLibrary :: IORef Core -> Maybe TLibs.Library -> IO ()
storeLibrary = libOperation (\ batchHandler (libID, _) -> do
    putStrLn "call storeLibrary - NOT YET IMPLEMENTED")


libraryRootDef :: IORef Core -> Maybe TLibs.Library -> IO TDefs.Definition
libraryRootDef = libOperation (\ batchHandler (_, library) -> do
    putStrLn "call libraryRootDef"
    core <- readIORef batchHandler
    let rootDefID' = Library.rootDefID library
        rootDef = Core.nodeDefByID core rootDefID'
    case rootDef of 
        Just rd -> do
                   let (trootDef, _) = encode (rootDefID', rd)
                   return trootDef
        Nothing -> throw' "Wrong `rootDefID` in `library` argument")

