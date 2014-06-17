---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.IO.BMP where

import qualified Codec.BMP                  as BMP
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.Array.Accelerate      as A
import qualified Data.Array.Accelerate.IO   as A
import           Data.Bits                  ((.&.))

import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Image   (Image)
import qualified Flowbox.Graphics.Image.Image   as Image
import qualified Flowbox.Graphics.Image.View    as View
import qualified Flowbox.Math.Matrix            as Matrix
import           Flowbox.Prelude


readFromBMP :: MonadIO m => FilePath -> m (Either BMP.Error (Image View.RGBA))
readFromBMP file = liftIO $ do
    rawData <- A.readImageFromBMP file
    return $ makeRGBA <$> rawData

makeRGBA :: A.Array A.DIM2 A.Word32 -> Image View.RGBA
makeRGBA array = image
    where word32Matrix   = Matrix.Raw array
          unpackedMatrix = Matrix.map unpack32 word32Matrix
          colors         = Matrix.unzip4 unpackedMatrix
          (r, g, b, a)   = colors & each %~ Channel.FlatData . Matrix.map A.fromIntegral
          finalView      = View.append (Channel.ChannelFloat "r" r) $
                           View.append (Channel.ChannelFloat "g" g) $
                           View.append (Channel.ChannelFloat "b" b) $
                           View.append (Channel.ChannelFloat "a" a) $
                           View.empty "rgba"
          image          = Image.singleton finalView


pack32 :: A.Exp (A.Word8, A.Word8, A.Word8, A.Word8) -> A.Exp A.RGBA32
pack32 rgba = r + g + b + a
    where (r', g', b', a')  = A.unlift rgba
          r                 = A.fromIntegral r'
          g                 = A.fromIntegral g' * 0x100
          b                 = A.fromIntegral b' * 0x10000
          a                 = A.fromIntegral a' * 0x1000000


unpack32 :: A.Exp A.RGBA32 -> A.Exp (A.Word8, A.Word8, A.Word8, A.Word8)
unpack32 rgba = A.lift (r, g, b, a)
   where r    = A.fromIntegral $ rgba                   .&. 0xFF
         g    = A.fromIntegral $ (rgba `div` 0x100)     .&. 0xFF
         b    = A.fromIntegral $ (rgba `div` 0x10000)   .&. 0xFF
         a    = A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF