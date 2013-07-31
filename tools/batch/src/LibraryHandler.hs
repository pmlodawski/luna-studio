---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module LibraryHandler (
    libraries,
    loadLibrary,
    unloadLibrary
) 
where
    
import           Control.Exception
import           Data.IORef
import qualified Data.Text.Lazy as Text
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)


import qualified Libs_Types
import           Batch_Types (MissingFieldsException(..))

import qualified Luna.Core           as Core
import qualified Luna.Lib.LibManager as LibManager
import           Luna.Lib.Library      (Library(..))
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Libs ()



libraries :: IORef Core.Core -> IO (Vector Libs_Types.Library)
libraries batchHandler = do 
    putStr "getting libraries...\t\t"
    core <- readIORef batchHandler
    let libManager =  Core.libManager core
        libs = LibManager.labNodes libManager
        tlibs = map encode libs
        tlibsVector = Vector.fromList tlibs
    
    putStrLn "success!"
    return tlibsVector

loadLibrary :: IORef Core.Core -> Maybe Libs_Types.Library -> IO Libs_Types.Library
loadLibrary batchHandler (Just tlibrary) = do
    putStr $ "loading library...\t" ++ (show tlibrary) ++ "\t"
    core <- readIORef batchHandler
    let 
        libManager = Core.libManager core
        [lID]      = LibManager.newNodes 1 libManager

    case decode tlibrary :: Either String (Int, Library) of
        Right (_, library) -> do 
                                  let newTLibrary = encode (lID, library)
                                      newCore = core { Core.libManager = LibManager.insNode (lID, library) libManager }
                                  writeIORef batchHandler newCore
                                  putStrLn "success!"
                                  return newTLibrary
        Left message       -> do
                                  putStrLn $ "failed: " ++ message
                                  throw $ MissingFieldsException $ Just $ Text.pack message

loadLibrary _ Nothing = do
    throw $ MissingFieldsException $ Just $ Text.pack "Library argument is missing";

unloadLibrary :: IORef Core.Core -> Maybe Libs_Types.Library -> IO ()
unloadLibrary batchHandler (Just tlibrary) = do
    putStr $ "unloading library...\t" ++ (show tlibrary) ++ "\t"
    core <- readIORef batchHandler
    let 
        libManager = Core.libManager core
 
    case decode tlibrary :: Either String (Int, Library) of
        Right (lID, _) -> do 
                                  let newCore = core { Core.libManager = LibManager.delNode lID libManager }
                                  writeIORef batchHandler newCore
                                  putStrLn "success!"
                                  return ()
        Left message       -> do
                                  putStrLn $ "failed: " ++ message
                                  throw $ MissingFieldsException $ Just $ Text.pack message

unloadLibrary _ Nothing = do
    throw $ MissingFieldsException $ Just $ Text.pack "Library argument is missing";
