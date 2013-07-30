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

import qualified Data.Vector as Vector
import           Data.Vector   (Vector)


libraries a = do 
    putStrLn "NOT IMPLEMENTED - libraries"
    return $ Vector.fromList []



loadLibrary a library   = putStrLn "NOT IMPLEMENTED - loadLibrary"


unloadLibrary a library = putStrLn "NOT IMPLEMENTED - unloadLibrary"