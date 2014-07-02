---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Flowbox.Graphics.Image.IO.ImageMagick where

import           Control.Monad.Trans                           (liftIO)
import           Control.Monad.Trans.Control                   (MonadBaseControl, liftBaseOp)
import           Control.Monad.Trans.Resource                  (MonadResource)
import qualified Data.Array.Accelerate                         as A
import qualified Data.Array.Accelerate.IO                      as A
import           Data.Bits                                     ((.&.), (.|.))
import qualified Graphics.ImageMagick.MagickWand               as Magick
import qualified Graphics.ImageMagick.MagickWand.FFI.WandImage as WandImage
import qualified Graphics.ImageMagick.MagickWand.Utils         as Magick (withException_)
import           Filesystem.Path.CurrentOS                     (decodeString)
import qualified Foreign.C.String                              as CString
import           Foreign.Ptr                                   (Ptr, castPtr)
import qualified Foreign.Marshal.Array                         as Array (allocaArray)
import           Foreign.Storable                              (Storable)

import           Flowbox.Prelude



getMagickPixels :: Magick.PMagickWand -> Int -> Int -> Int -> Int -> String -> IO (Ptr A.Word8)
getMagickPixels w x y width height cmap = CString.withCString cmap $ \cstr ->
    Array.allocaArray arrLength $ \q ->
        WandImage.magickExportImagePixels w x' y' width' height' cstr Magick.charPixel (castPtr q) >> return q
    where 
        x' = fromIntegral x
        y' = fromIntegral y
        width'  = fromIntegral width
        height' = fromIntegral height
        arrLength = width * height * length cmap

setMagickPixels :: MonadResource m => Magick.PMagickWand -> Int -> Int -> Int -> Int -> String -> Ptr A.Word8 -> m ()
setMagickPixels w x y width height cmap pixels = Magick.withException_ w $ CString.withCString cmap $ \cstr ->
    WandImage.magickImportImagePixels w x' y' width' height' cstr Magick.charPixel (castPtr pixels)
    where
      x' = fromIntegral x
      y' = fromIntegral y
      width'  = fromIntegral width
      height' = fromIntegral height

fromImageMagick :: Int -> Int -> A.Acc (A.Vector A.Word8) -> A.Acc (A.Array A.DIM2 A.RGBA32)
fromImageMagick width height input = A.generate size $ \i ->
        let A.Z A.:. y A.:. x = A.unlift i :: A.Z A.:. A.Exp Int A.:. A.Exp Int
        in imagick x y
    where 
        w = A.the $ A.unit $ A.lift width
        h = A.the $ A.unit $ A.lift height
        size = A.index2 h w
        -- FIXME[mm]: pack32 maybe?
        imagick x y =  pos 0 `A.shiftL` 0 
                   .|. pos 1 `A.shiftL` 8
                   .|. pos 2 `A.shiftL` 16
                   .|. pos 3 `A.shiftL` 24
            where pos o = A.fromIntegral $ input A.!! (4 * (((h + 1) - y) * w + x) + o)

toImageMagick :: A.Acc (A.Array A.DIM2 A.RGBA32) -> A.Acc (A.Vector A.Word8, A.Scalar Int, A.Scalar Int)
toImageMagick input = A.lift (A.generate (A.index1 newlen) $ \i' ->
        let A.Z A.:. i = A.unlift i' :: A.Z A.:. A.Exp Int
        in imagick i, A.unit width, A.unit height)
    where 
        oldsize = A.shape input
        A.Z A.:. height A.:. width = A.unlift oldsize :: A.Z A.:. A.Exp Int A.:. A.Exp Int
        newlen = A.shapeSize oldsize * 4
        imagick i = let item_x = i `div` 4
                        item_y = (4 * ((height + 1) * width + item_x) + offset - i) `div` (4 * width)
                        item = input A.! A.index2 item_y item_x
                        offset = 8 * (i `mod` 4)
                    in A.fromIntegral $ (item `A.shiftR` offset) .&. 255

loadImage :: FilePath -> IO (A.Acc (A.Array A.DIM2 A.RGBA32))
loadImage filename = Magick.withMagickWandGenesis $ do
    (_, mWand) <- Magick.magickWand
    Magick.readImage mWand $ decodeString filename
    width  <- Magick.getImageWidth mWand
    height <- Magick.getImageHeight mWand

    let size = width * height * 4
    pixels <- liftIO $ getMagickPixels mWand 0 0 width height "RGBA"
    pixArray <- liftIO $ A.fromPtr (A.Z A.:. size) ((), pixels)

    return $ fromImageMagick width height (A.use pixArray)

--saveImage :: Matrix.Backend -> FilePath -> A.Acc (A.Array A.DIM2 A.RGBA32) -> IO ()
saveImage backend filename arr = Magick.withMagickWandGenesis $ do
    (_, mWand) <- Magick.magickWand

    let (magick, width', height') = backend $ toImageMagick arr
        [width] = A.toList width'
        [height] = A.toList height'
    Magick.setSize mWand width height
    Magick.readImage mWand "xc:white"

    let arrLength = A.arraySize $ A.arrayShape magick
    allocaArray arrLength $ \pixels -> do
        liftIO $ A.toPtr magick ((), pixels)
        setMagickPixels mWand 0 0 width height "RGBA" pixels
        Magick.writeImage mWand (Just $ decodeString filename)

allocaArray :: (MonadBaseControl IO m, Storable a) => Int -> (Ptr a -> m b) -> m b
allocaArray = liftBaseOp . Array.allocaArray
