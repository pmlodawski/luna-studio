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

import           Data.IORef
import qualified Data.Vector as Vector
import           Data.Vector   (Vector)

import qualified Luna.Core   as Core
import qualified Luna.Lib.LibManager as LibManager
import qualified Luna.Lib.Library    as Library

libraries batchHandler = do 
    core <- readIORef batchHandler
    let
        libManager = Core.libManager core
    putStrLn "NOT IMPLEMENTED - libraries"
    return $ Vector.fromList []


loadLibrary batchHandler (Just library) = do
    putStrLn "NOT IMPLEMENTED - loadLibrary"

    core <- readIORef batchHandler
    let 
        libManager = Core.libManager core
        [lID] = LibManager.newNodes 1 libManager
        newCore = core { Core.libManager = LibManager.insNode (lID, Library.empty) libManager  }
    putStrLn $ "Library added with LibID = " ++ show lID
    writeIORef batchHandler newCore
    
    return ()


unloadLibrary batchHandler (Just library) = do
    putStrLn "NOT IMPLEMENTED - unloadLibrary"
    
    core <- readIORef batchHandler
    let 
        libManager = Core.libManager core
        newCore = core --{ Core.libManager = LibManager.delNode ... }
    writeIORef batchHandler newCore
    
    return ()
