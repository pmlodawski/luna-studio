---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Codec.EXR.Internal.Bindings where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Flowbox.Codec.EXR.Internal.Types



-- File I/O
foreign import ccall unsafe "haskexr.h openFile"   openFile  :: CString -> IO (Ptr EXR)
foreign import ccall unsafe "haskexr.h &closeFile" closeFile :: FunPtr (Ptr EXR -> IO ())

-- Multi-part file properties
foreign import ccall "haskexr.h parts"   parts   :: Ptr EXR -> IO CInt
foreign import ccall "haskexr.h version" version :: Ptr EXR -> IO CInt

foreign import ccall "haskexr.h displayWindowUnsafe" displayWindowUnsafe ::
    Ptr EXR -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "haskexr.h dataWindowUnsafe" dataWindowUnsafe ::
    Ptr EXR -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "haskexr.h channels" channels ::
    Ptr EXR -> CInt -> Ptr CInt -> IO (Ptr CChar)

-- Various attributes accesors
foreign import ccall "haskexr.h getPartType" getPartType :: Ptr EXR -> CInt -> IO (CString)
foreign import ccall "haskexr.h getPartName" getPartName :: Ptr EXR -> CInt -> IO (CString)
foreign import ccall "haskexr.h getPartVersion" getPartVersion :: Ptr EXR -> CInt -> Ptr CInt -> IO (CInt)
foreign import ccall "haskexr.h getPartView" getPartView :: Ptr EXR -> CInt -> IO (CString)
foreign import ccall "haskexr.h getPixelAspectRatio" getPixelAspectRatio :: Ptr EXR -> CInt -> IO (CFloat)
foreign import ccall "haskexr.h getScreenWindowWidth" getScreenWindowWidth :: Ptr EXR -> CInt -> IO (CFloat)
foreign import ccall "haskexr.h getScreenWindowCenter" getScreenWindowCenter :: Ptr EXR -> CInt -> Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall "haskexr.h readScanlineChannelUnsafe" readScanlineChannel ::
    Ptr EXR -> CInt -> CString -> Ptr CInt -> Ptr CInt -> IO (Ptr CFloat)

foreign import ccall unsafe "haskexr.h readTileFromChannelUnsafe" readTileFromChannel ::
	Ptr EXR -> CInt -> CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr CFloat)
	--Ptr EXR -> CInt -> CString -> Ptr CInt -> Ptr CInt -> IO (Ptr CFloat)
foreign import ccall "haskexr.h readTiledScanlineChannelUnsafe" readTiledScanlineChannel ::
	Ptr EXR -> CInt -> CString -> Ptr CInt -> Ptr CInt -> IO (Ptr CFloat)

foreign import ccall "haskexr.h foobar" foobar ::
	Ptr EXR -> CInt -> CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr CFloat)

-- not supported
--foreign import ccall "haskexr.h readDeepScanlineChannelUnsafe" readDeepScanlineChannel ::
--    Ptr EXR -> CInt -> CString -> Ptr CInt -> Ptr CInt -> IO (Ptr CFloat)


-- |Debug function useful for showing info about particular OpenEXR file
foreign import ccall "haskexr.h dumpImageInfo" dumpImageInfo :: Ptr EXR -> IO ()
