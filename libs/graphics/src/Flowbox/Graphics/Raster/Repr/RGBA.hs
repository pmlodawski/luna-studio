---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Raster.Repr.RGBA where

import           Data.Array.Accelerate    (Exp)
import qualified Data.Array.Accelerate    as A
import qualified Data.Array.Accelerate.IO as A
import           Data.Bits                ((.&.))

import           Flowbox.Graphics.Raster.Channel (RawData2D, RawData3D)
import qualified Flowbox.Graphics.Raster.Channel as Channel
import           Flowbox.Graphics.Raster.Image   (Image)
import qualified Flowbox.Graphics.Raster.Image   as Image
import           Flowbox.Prelude                 hiding (map)


decompose :: (A.Shape ix) => Image (A.Array ix A.Word32) -> Either Image.Error (Image (A.Array ix A.Word8))
decompose img = do chan <- Image.lookup "rgba" img
                   let dchan = Channel.map unpack32 chan
                       (r,g,b,a) = Channel.unzip4 dchan
                       outimg    = Image.insert "r" r
                                 $ Image.insert "g" g
                                 $ Image.insert "b" b
                                 $ Image.insert "a" a
                                 $ mempty
                   return outimg

compose :: (A.Shape ix) => Image (A.Array ix A.Word8) -> Either Image.Error (Image (A.Array ix A.Word32))
compose img = do r <- Image.lookup "r" img
                 g <- Image.lookup "g" img
                 b <- Image.lookup "b" img
                 a <- Image.lookup "a" img

                 let rgba = Channel.map pack32 (Channel.zip4 r g b a)
                 return $ Image.insert "rgba" rgba mempty


pack32 :: Exp (A.Word8, A.Word8, A.Word8, A.Word8) -> Exp A.RGBA32
pack32 rgba = r + g + b + a
    where (r', g', b', a')  = A.unlift rgba
          r                 = A.fromIntegral r'
          g                 = (A.fromIntegral g') * 0x100
          b                 = (A.fromIntegral b') * 0x10000
          a                 = (A.fromIntegral a') * 0x1000000


unpack32 :: Exp A.RGBA32 -> Exp (A.Word8, A.Word8, A.Word8, A.Word8)
unpack32 rgba = A.lift (r, g, b, a)
   where r    = A.fromIntegral $ rgba                   .&. 0xFF
         g    = A.fromIntegral $ (rgba `div` 0x100)     .&. 0xFF
         b    = A.fromIntegral $ (rgba `div` 0x10000)   .&. 0xFF
         a    = A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF
