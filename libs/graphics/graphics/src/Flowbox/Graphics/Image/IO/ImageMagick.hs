---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.IO.ImageMagick where

import qualified Control.Exception.Lifted                      as Exception
import           Control.Monad.Trans                           (liftIO)
import           Control.Monad.Trans.Control                   (MonadBaseControl, liftBaseOp)
import           Control.Monad.Trans.Resource                  (MonadResource)
import qualified Data.Array.Accelerate                         as A
import qualified Data.Array.Accelerate.IO                      as A
import           Graphics.ImageMagick.MagickCore.Exception     (MagickWandException)
import qualified Graphics.ImageMagick.MagickWand               as Magick
import qualified Graphics.ImageMagick.MagickWand.FFI.WandImage as WandImage
import qualified Graphics.ImageMagick.MagickWand.Utils         as Magick (withException_)
import           Filesystem.Path.CurrentOS                     (decodeString)
import qualified Foreign.C.String                              as CString
import           Foreign.Ptr                                   (Ptr, castPtr)
import qualified Foreign.Marshal.Alloc                         as Alloc (allocaBytes)

import Flowbox.Prelude



loadImage :: FilePath -> IO (Either MagickWandException (A.Array A.DIM2 A.RGBA32))
loadImage filename = tryMagick $ Magick.withMagickWandGenesis $ do
    (_, mWand) <- Magick.magickWand
    Magick.readImage mWand $ decodeString filename
    width <- Magick.getImageWidth mWand
    height <- Magick.getImageHeight mWand

    allocaBytes (width * height * 4) $ \mem -> do
        readImagePixelsToPtr mWand width height mem

        liftIO $ A.fromPtr (A.Z A.:. height A.:. width) ((), mem)

readImagePixelsToPtr :: (MonadBaseControl IO m, MonadResource m) => Magick.PMagickWand -> Int -> Int -> Ptr A.Word32 -> m ()
readImagePixelsToPtr wand width height mem = withCString "RGBA" $ \pixelMap ->
    Magick.withException_ wand $
        WandImage.magickExportImagePixels wand 0 0 (fromIntegral width) (fromIntegral height) pixelMap Magick.charPixel (castPtr mem)

saveImage :: FilePath -> A.Array A.DIM2 A.RGBA32 -> IO ()
saveImage filename arr = Magick.withMagickWandGenesis $ do
    (_, mWand) <- Magick.magickWand

    let size@(A.Z A.:. height A.:. width) = A.arrayShape arr
        arrayLength = A.arraySize size

    Magick.setSize mWand width height
    Magick.readImage mWand "xc:white"

    allocaBytes (4 * arrayLength) $ \mem -> do
        liftIO $ A.toPtr arr ((), mem)
        flip catchMagick (\e -> liftIO $ print e) $ do
            usePixelsFromPtr mWand width height mem
            Magick.writeImage mWand (Just $ decodeString filename)

usePixelsFromPtr :: (MonadBaseControl IO m, MonadResource m) => Magick.PMagickWand -> Int -> Int -> Ptr A.Word32 -> m ()
usePixelsFromPtr wand width height mem = withCString "RGBA" $ \pixelMap ->
    Magick.withException_ wand $
        WandImage.magickImportImagePixels wand 0 0 (fromIntegral width) (fromIntegral height) pixelMap Magick.charPixel (castPtr mem)

-- utils, belong probably in lifted-base

allocaBytes :: (MonadBaseControl IO m) => Int -> (Ptr a -> m b) -> m b
allocaBytes = liftBaseOp . Alloc.allocaBytes

withCString :: (MonadBaseControl IO m) => String -> (CString.CString -> m a) -> m a
withCString = liftBaseOp . CString.withCString

tryMagick :: MonadBaseControl IO m => m a -> m (Either MagickWandException a)
tryMagick = Exception.try

catchMagick :: MonadBaseControl IO m => m a -> (MagickWandException -> m a) -> m a
catchMagick = Exception.catch
