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

import           Data.Int
import           Data.IORef
import qualified Data.Vector as Vector
import           Data.Vector   (Vector)

import qualified Libs_Types

import qualified Luna.Core           as Core
import qualified Luna.Lib.LibManager as LibManager
import qualified Luna.Lib.Library    as Library
import           Luna.Lib.Library      (Library(..))
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.LibsSerialization



libraries batchHandler = do 
    putStr "getting libraries...\t\t"
    core <- readIORef batchHandler
    let libManager =  Core.libManager core
        --libraries = LibManager.labNodes libManager
        --tlibraries = map encode libraries
        --tlibrariesVector = Vector.fromList tlibraries
    
    putStrLn "failed! NOT IMPLEMENTED"
    --return tlibrariesVector
    return $ Vector.fromList []


loadLibrary batchHandler (Just tlibrary) = do
    putStr "loading library...\t\t"
    core <- readIORef batchHandler
    let 
        libManager = Core.libManager core
        [lID]      = LibManager.newNodes 1 libManager

        Right (_, library) = decode tlibrary :: Either String (Int, Library)

        newCore = core { Core.libManager = LibManager.insNode (lID, library) libManager  }
    writeIORef batchHandler newCore

    putStrLn "success!"
    return ()


unloadLibrary batchHandler (Just tlibrary) = do
    putStr "unloading library...\t\t"
    core <- readIORef batchHandler
    let 
        libManager = Core.libManager core
        Right (lID, _) = decode tlibrary :: Either String (Int, Library)
        newCore = core { Core.libManager = LibManager.delNode lID libManager }
    writeIORef batchHandler newCore
    
    putStrLn "success!"
    return ()

