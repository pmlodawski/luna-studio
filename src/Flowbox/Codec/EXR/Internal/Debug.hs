---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Codec.EXR.Internal.Debug (
	dumpImageInfo
	) where

import Foreign.ForeignPtr

import qualified Flowbox.Codec.EXR.Internal.Bindings as Bindings
import           Flowbox.Codec.EXR.Internal.Types



-- |Dumps all available info about an OpenEXR file to stdout.
dumpImageInfo :: EXRFile -> IO ()
dumpImageInfo (EXRFile exr) = withForeignPtr exr $ \ptr -> Bindings.dumpImageInfo ptr
