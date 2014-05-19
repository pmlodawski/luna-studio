---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Color.Convert.YUV_HD (
    ColorSpaceAccYUV_HD
) where

import           Data.Array.Accelerate
import           Data.Map              (Map)

import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.RGB               as Color
import           Flowbox.Graphics.Color.RGBA              as Color
import           Flowbox.Graphics.Color.HSV               as Color
import           Flowbox.Graphics.Color.HSL               as Color
import           Flowbox.Graphics.Color.CMY               as Color
import           Flowbox.Graphics.Color.CMYK              as Color
import           Flowbox.Graphics.Color.YUV               as Color
import           Flowbox.Graphics.Color.YUV_HD            as Color
import           Flowbox.Graphics.Color.YUV_HD.Conversion as Color
import           Flowbox.Graphics.Image                   (Image)
import qualified Flowbox.Graphics.Image                   as Image
import           Flowbox.Graphics.Image.Color.Helpers
import           Flowbox.Graphics.Image.Channel           (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel           as Channel
import           Flowbox.Graphics.Image.ImageRGB          (ImageRGB(..))
import           Flowbox.Graphics.Image.ImageRGBA         (ImageRGBA(..))
import           Flowbox.Graphics.Image.ImageHSV          (ImageHSV(..))
import           Flowbox.Graphics.Image.ImageHSL          (ImageHSL(..))
import           Flowbox.Graphics.Image.ImageCMY          (ImageCMY(..))
import           Flowbox.Graphics.Image.ImageCMYK         (ImageCMYK(..))
import           Flowbox.Graphics.Image.ImageYUV          (ImageYUV(..))
import           Flowbox.Graphics.Image.ImageYUV_HD       (ImageYUV_HD(..))
import           Flowbox.Prelude



class ColorSpaceAccYUV_HD img where
    toYUV_HD :: (Shape ix, Elt a, IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageYUV_HD (ChannelAcc ix a))

instance ColorSpaceAccYUV_HD ImageYUV_HD where
    toYUV_HD = return . id

instance ColorSpaceAccYUV_HD ImageRGB where
    toYUV_HD img@(ImageRGB chans) = threeWayConvert' Color.RGB ("rgb.r", "rgb.g", "rgb.b") img chans

instance ColorSpaceAccYUV_HD ImageRGBA where
    toYUV_HD img@(ImageRGBA chans) = convertFromFour ("yuv_hd.y", "yuv_hd.u", "yuv_hd.v") ("rgba.r", "rgba.g", "rgba.b", "rgba.a") (convertFourToYUV_HD Color.RGBA) ImageYUV_HD img chans

instance ColorSpaceAccYUV_HD ImageHSV where
    toYUV_HD img@(ImageHSV chans) = threeWayConvert' Color.HSV ("hsv.h", "hsv.s", "hsv.v") img chans

instance ColorSpaceAccYUV_HD ImageHSL where
    toYUV_HD img@(ImageHSL chans) = threeWayConvert' Color.YUV_HD ("hsl.h", "hsl.s", "hsl.l") img chans

instance ColorSpaceAccYUV_HD ImageCMY where
    toYUV_HD img@(ImageCMY chans) = threeWayConvert' Color.CMY ("cmy.c", "cmy.m", "cmy.y") img chans

instance ColorSpaceAccYUV_HD ImageCMYK where
    toYUV_HD img@(ImageCMYK chans) = convertFromFour ("yuv_hd.y", "yuv_hd.u", "yuv_hd.v") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") (convertFourToYUV_HD Color.CMYK) ImageYUV_HD img chans

instance ColorSpaceAccYUV_HD ImageYUV where
    toYUV_HD img@(ImageYUV chans) = threeWayConvert' Color.YUV ("yuv.y", "yuv.u", "yuv.v") img chans


-- = Helpers

threeWayConvert' convertFrom = threeWayConvert ImageYUV_HD ("yuv_hd.y", "yuv_hd.u", "yuv_hd.v") (convertToYUV_HD convertFrom)

convertFourToYUV_HD :: (Elt a, IsFloating a, Unlift c (t, t1, t2, t3), Lift c2 (Exp a, Exp a, Exp a), ColorConvertAcc c1 YUV_HD)
    => (t -> t1 -> t2 -> t3 -> c1 (Exp a)) -> c (Plain t, Plain t1, Plain t2, Plain t3)
    -> c2 (a, a, a)
convertFourToYUV_HD fromSpace cmyk' = lift (h, s, v)
    where (c', m', y', k') = unlift cmyk'
          Color.YUV_HD h s v = Color.toYUV_HD $ fromSpace c' m' y' k'

convertToYUV_HD :: (Elt a, IsFloating a, Unlift b (x, y, z), Lift c (Exp a, Exp a, Exp a), ColorConvertAcc c1 YUV_HD)
    => (x -> y -> z -> c1 (Exp a)) -> b (Plain x, Plain y, Plain z) -> c (a, a, a)
convertToYUV_HD fromSpace zxc' = lift (r, g, b)
    where (z', x', c') = unlift zxc'
          Color.YUV_HD r g b = Color.toYUV_HD $ fromSpace z' x' c'

convertCMYKtoYUV_HD :: (Elt a, IsFloating a, Unlift c (Exp a, Exp a, Exp a, Exp a), Lift c1 (Exp a, Exp a, Exp a))
    => c (Plain (Exp a), Plain (Exp a), Plain (Exp a), Plain (Exp a)) -> c1 (a, a, a)
convertCMYKtoYUV_HD cmyk' = lift (r, g, b)
    where (c', m', y', k') = unlift cmyk'
          Color.YUV_HD r g b = Color.toYUV_HD $ Color.CMYK c' m' y' k'
