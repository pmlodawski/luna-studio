---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Codec.EXR.Internal.IO where

import Foreign
import Foreign.C.String

import qualified Flowbox.Codec.EXR.Internal.Bindings as Bindings
import 			 Flowbox.Codec.EXR.Internal.Types



openEXRFile :: FilePath -> IO (Maybe EXRFile)
openEXRFile filePath = do
    filePtr <- withCString filePath $ \path -> do
        Bindings.openFile path
    if filePtr == nullPtr
        then return Nothing
        else do
            fptr <- newForeignPtr Bindings.closeFile filePtr
            return $ Just $ EXRFile $ fptr
