{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Raster.Image.IO where

import           Flowbox.Prelude               hiding(map)

import qualified Data.Array.Accelerate         as A
import           Data.Array.Accelerate         (Acc, Exp, (:.)(..))
import qualified Data.Array.Accelerate.IO      as A
import           Data.Bits                     ((.&.))
import           Data.Monoid                   (mempty, Monoid)
import qualified Codec.BMP                     as BMP
import           Control.Applicative
import           Control.Monad.IO.Class        (MonadIO, liftIO)

import qualified Flowbox.Graphics.Raster.Image    as Image
import           Flowbox.Graphics.Raster.Image    (Image)
import qualified Flowbox.Graphics.Raster.Channel  as Channel
import           Flowbox.Graphics.Raster.Channel  (Channel)
import           Control.Monad.Trans.Either       (runEitherT, hoistEither)


readImageFromBMP :: MonadIO m => FilePath -> m (Either BMP.Error (Image A.Word8))
readImageFromBMP file = liftIO(fmap decodeRGBA32 <$> A.readImageFromBMP file)



readImageFromBMP' :: MonadIO m => FilePath -> m (Either BMP.Error (Image A.Word32))
readImageFromBMP' file = liftIO(fmap mkChan <$> A.readImageFromBMP file) where
    mkChan chdata = Image.insert "rgba" (Channel.Raw chdata) mempty

decomposeRGBA :: Image A.Word32 -> Either Image.Error (Image A.Word8)
decomposeRGBA img = do chan <- Image.lookup' "rgba" img
                       let getChan f = Channel.map f chan
                           getR rgba = A.fromIntegral $ rgba .&. 0xFF
                           getG rgba = A.fromIntegral $ (rgba `div` 0x100) .&. 0xFF
                           getB rgba = A.fromIntegral $ (rgba `div` 0x10000) .&. 0xFF
                           getA rgba = A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF
                           outimg    = Image.insert "red"   ( getChan getR )
                                     $ Image.insert "green" ( getChan getG )
                                     $ Image.insert "blue"  ( getChan getB )
                                     $ Image.insert "alpha" ( getChan getA )
                                     $ mempty
                       return outimg



writeImageToBMP :: MonadIO m => Channel.Backend A.Word32 -> FilePath -> Image A.Word8 -> m (Either Image.Error ())
writeImageToBMP backend file img = runEitherT $ do 
    chan <- hoistEither $ (encodeRGBA32' img)
    let Channel.Raw mdata = Channel.compute backend chan
    liftIO $ A.writeImageToBMP file mdata


decodeRGBA32 :: A.Array A.DIM2 A.RGBA32 -> Image A.Word8
decodeRGBA32 (A.use -> img) = Image.insert "red"   ( getChan getR )
                            $ Image.insert "green" ( getChan getG )
                            $ Image.insert "blue"  ( getChan getB )
                            $ Image.insert "alpha" ( getChan getA )
                            $ mempty
                            where getChan f = Channel.Acc $ A.map f img
                                  getR rgba = A.fromIntegral $ rgba .&. 0xFF
                                  getG rgba = A.fromIntegral $ (rgba `div` 0x100) .&. 0xFF
                                  getB rgba = A.fromIntegral $ (rgba `div` 0x10000) .&. 0xFF
                                  getA rgba = A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF


encodeRGBA32 :: Image A.Word8 -> Maybe(Acc(A.Array A.DIM2 A.RGBA32))
encodeRGBA32 img = let r = fmap Channel.use $ Image.lookup "red"   img
                       g = fmap Channel.use $ Image.lookup "green" img
                       b = fmap Channel.use $ Image.lookup "blue"  img
                       a = fmap Channel.use $ Image.lookup "alpha" img
                   in A.map packRGBA32 <$> (A.zip4 <$> r <*> g <*> b <*> a)

encodeRGBA32' :: Image A.Word8 -> Either Image.Error (Channel A.Word32)
encodeRGBA32' img = do r <- Image.lookup' "red"   img
                       g <- Image.lookup' "green" img
                       b <- Image.lookup' "blue"  img
                       a <- Image.lookup' "alpha" img
                       return $ Channel.map packRGBA32 (Channel.zip4 r g b a)


packRGBA32 :: Exp (A.Word8, A.Word8, A.Word8, A.Word8) -> Exp A.RGBA32
packRGBA32 rgba =
  let (r', g', b', a')  = A.unlift rgba
      r                 = A.fromIntegral r'
      g                 = (A.fromIntegral g') * 0x100
      b                 = (A.fromIntegral b') * 0x10000
      a                 = (A.fromIntegral a') * 0x1000000
  in
  r + g + b + a

--getRfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word8
--getRfromRGBA32 rgba = A.fromIntegral $ rgba .&. 0xFF

--getGfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word8
--getGfromRGBA32 rgba = A.fromIntegral $ (rgba `div` 0x100) .&. 0xFF

--getBfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word8
--getBfromRGBA32 rgba = A.fromIntegral $ (rgba `div` 0x10000) .&. 0xFF

--getAfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word8
--getAfromRGBA32 rgba = A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF

