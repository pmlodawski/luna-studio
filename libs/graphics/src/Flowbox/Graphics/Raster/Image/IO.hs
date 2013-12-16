{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Raster.Image.IO where

import           Prelude                       hiding(map)

import qualified Data.Array.Accelerate         as A
import           Data.Array.Accelerate         (Acc, Exp, (:.)(..))
import qualified Data.Array.Accelerate.CUDA    as CUDA
import qualified Data.Array.Accelerate.IO      as A
import           Data.Bits                     ((.&.))
import           Data.Monoid                   (mempty, Monoid)
import qualified Codec.BMP                     as BMP
import           Control.Applicative

import qualified Flowbox.Graphics.Raster.Image    as Image
import           Flowbox.Graphics.Raster.Image    (Image)
import qualified Flowbox.Graphics.Raster.Channel  as Channel
import           Flowbox.Graphics.Raster.Channel  (Channel)


readImageFromBMP :: FilePath -> IO (Either BMP.Error (Image A.Word8))
readImageFromBMP file = fmap decodeRGBA32 <$> A.readImageFromBMP file


decodeRGBA32 :: A.Array A.DIM2 A.RGBA32 -> Image A.Word8
decodeRGBA32 (A.use -> img) = Image.insert "red"   ( getChan getRfromRGBA32 )
                            $ Image.insert "green" ( getChan getGfromRGBA32 )
                            $ Image.insert "blue"  ( getChan getBfromRGBA32 )
                            $ Image.insert "alpha" ( getChan getAfromRGBA32 )
                            $ mempty
                            where getChan f = Channel.Acc $ A.map f img


encodeRGBA32 :: Image A.Word8 -> Maybe(Acc(A.Array A.DIM2 A.RGBA32))
encodeRGBA32 img = let r = fmap Channel.use $ Image.lookup "red"   img
                       g = fmap Channel.use $ Image.lookup "green" img
                       b = fmap Channel.use $ Image.lookup "blue"  img
                       a = fmap Channel.use $ Image.lookup "alpha" img
                   in A.map packRGBA32 <$> (A.zip4 <$> r <*> g <*> b <*> a)


packRGBA32 :: Exp (A.Word8, A.Word8, A.Word8, A.Word8) -> Exp A.RGBA32
packRGBA32 rgba =
  let (r', g', b', a')  = A.unlift rgba
      r                 = A.fromIntegral r'
      g                 = (A.fromIntegral g') * 0x100
      b                 = (A.fromIntegral b') * 0x10000
      a                 = (A.fromIntegral a') * 0x1000000
  in
  r + g + b + a

getRfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word8
getRfromRGBA32 rgba = A.fromIntegral $ rgba .&. 0xFF

getGfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word8
getGfromRGBA32 rgba = A.fromIntegral $ (rgba `div` 0x100) .&. 0xFF

getBfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word8
getBfromRGBA32 rgba = A.fromIntegral $ (rgba `div` 0x10000) .&. 0xFF

getAfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word8
getAfromRGBA32 rgba = A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF

