---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Codec.EXR.Internal.Attributes (
      getParts
    , getVersion
    , getChannels
    , getDisplayWindowUnsafe
    , getDataWindowUnsafe
    , getPartName
    , getPartView
    , getPartVersion
    , getPartType
    , getPixelAspectRatio
    , getScreenWindowCenter
    , getScreenWindowWidth
    , ChannelName
    , Header(..)
    , readHeader
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (forM)
import Data.List.Split     (endBy)
import Foreign
import Foreign.C.String
import Foreign.C.Types

import qualified Flowbox.Codec.EXR.Internal.Bindings as Bindings
import           Flowbox.Codec.EXR.Internal.Types



-- |Reads a number of parts that are available in an OpenEXR file.
getParts :: EXRFile -> IO Int
getParts (EXRFile exr) = do
    parts' <- withForeignPtr exr $ \ptr -> Bindings.parts ptr
    return $ fromIntegral parts'


-- |Reads a version of an OpenEXR file as Int. Currently there is no way to
--  query what properties are encoded in the return value.
getVersion :: EXRFile -> IO Int
getVersion (EXRFile exr) = do
    version' <- withForeignPtr exr $ \ptr -> Bindings.version ptr
    return $ fromIntegral version'


-- |Returns a list of channels names available in a part of an OpenEXR file.
getChannels :: EXRFile -> PartNumber -> IO [String]
getChannels (EXRFile exr) part = do
    namesCString <- withForeignPtr exr $ \ptr ->
        alloca $ \len -> do
            namesPtr <- Bindings.channels ptr (fromIntegral part) len
            namesLen <- peek len
            return (namesPtr, fromIntegral namesLen)
    str <- peekCStringLen namesCString
    let (cstringPtr, _) = namesCString
    free cstringPtr
    return $ endBy "\0" str


-- |Reads a displayWindow attribute from OpenEXR file part
getDisplayWindowUnsafe :: EXRFile -> PartNumber -> IO Box
getDisplayWindowUnsafe = marshallBox Bindings.displayWindowUnsafe


-- |Reads a dataWindow attribute from OpenEXR file part
getDataWindowUnsafe :: EXRFile -> PartNumber -> IO Box
getDataWindowUnsafe = marshallBox Bindings.dataWindowUnsafe


marshallBox :: (Ptr EXR -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()) -> EXRFile -> PartNumber -> IO Box
marshallBox foreignFun (EXRFile exr) part = do
    box <- withForeignPtr exr $ \ptr ->
        alloca $ \minx ->
        alloca $ \miny ->
        alloca $ \maxx ->
        alloca $ \maxy -> do
            foreignFun ptr (fromIntegral part) minx miny maxx maxy
            Box <$>
                (Point <$>
                    (fromIntegral <$> peek minx) <*> (fromIntegral <$> peek miny)) <*>
                (Point <$>
                    (fromIntegral <$> peek maxx) <*> (fromIntegral <$> peek maxy))
    return box


-- |Reads a part name from an OpenEXR file.
getPartName :: EXRFile -> PartNumber -> IO (Maybe String)
getPartName = marshallCString Bindings.getPartName


-- |Reads a part view from an OpenEXR file.
getPartView :: EXRFile -> PartNumber -> IO (Maybe String)
getPartView = marshallCString Bindings.getPartView


-- |Reads a part version from an OpenEXR file.
getPartVersion :: EXRFile -> PartNumber -> IO (Maybe Int)
getPartVersion (EXRFile exr) part = do
    (status, ver) <- withForeignPtr exr $ \ptr ->
        alloca $ \ver -> do
            status <- Bindings.getPartVersion ptr (fromIntegral part) ver
            ver' <- peek ver
            return (status, ver')
    if status == 0
        then
            return $ Just $ fromIntegral ver
        else
            return Nothing


getPartType :: EXRFile -> PartNumber -> IO (Maybe PartType)
getPartType = (fmap.fmap.fmap) toPartType . marshallCString Bindings.getPartType
    where toPartType str = case str of
                           "scanlineimage" -> ScanlineImage
                           "tiledimage"    -> TiledImage
                           "deepscanline"  -> DeepScanline
                           "deeptile"      -> DeepTile
                           _               -> Unknown str

marshallCString :: (Ptr EXR -> CInt -> IO (CString)) -> EXRFile -> PartNumber -> IO (Maybe String)
marshallCString foreignFun (EXRFile exr) part = do
    cstring <- withForeignPtr exr $ \ptr -> foreignFun ptr (fromIntegral part)
    if cstring == nullPtr
        then
            return Nothing
        else do
            str <- peekCString cstring
            free cstring
            return $ Just str


getPixelAspectRatio :: EXRFile -> PartNumber -> IO Float
getPixelAspectRatio (EXRFile exr) part = do
    par <- withForeignPtr exr $ \ptr -> Bindings.getPixelAspectRatio ptr (fromIntegral part)
    return $ realToFrac par


getScreenWindowWidth :: EXRFile -> PartNumber -> IO Float
getScreenWindowWidth (EXRFile exr) part = do
    sww <- withForeignPtr exr $ \ptr -> Bindings.getScreenWindowWidth ptr (fromIntegral part)
    return $ realToFrac sww


getScreenWindowCenter :: EXRFile -> PartNumber -> IO (Point Float)
getScreenWindowCenter (EXRFile exr) part = do
    (x', y') <- withForeignPtr exr $ \ptr ->
        alloca $ \xptr ->
        alloca $ \yptr -> do
            Bindings.getScreenWindowCenter ptr (fromIntegral part) xptr yptr
            x' <- peek xptr
            y' <- peek yptr
            return (x', y')
    return $ Point (realToFrac x') (realToFrac y')


type ChannelName = String


-- |Haskell representation of an OpenEXR file header.
data Header = Header { displayWindow      :: Box
                     , dataWindow         :: Box
                     , pixelAspectRatio   :: Float
                     , screenWindowWidth  :: Float
                     , screenWindowCenter :: Point Float
                     , channels           :: [ChannelName]
                     , name               :: Maybe String
                     , typ                :: Maybe PartType
                     , version            :: Maybe Int
                     , view               :: Maybe String
                     } deriving Show


-- |Convenience function for reading all headers in a multi-part OpenEXR file at once.
readHeader :: EXRFile -> IO [Header]
readHeader exr = do
    fileParts <- getParts exr
    forM [0..fileParts-1] $ \index -> do
        displayWin <- getDisplayWindowUnsafe exr index
        dataWin    <- getDataWindowUnsafe exr index
        scrWidth   <- getScreenWindowWidth exr index
        winCenter  <- getScreenWindowCenter exr index
        pixRatio   <- getPixelAspectRatio exr index
        chans <- getChannels exr index
        nam   <- getPartName exr index
        typ'  <- getPartType exr index
        vers' <- getPartVersion exr index
        view' <- getPartView exr index
        return $ Header displayWin dataWin pixRatio scrWidth winCenter chans nam typ' vers' view'