---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Color.Convert.YUV (
    ColorSpaceAccYUV
) where

import           Data.Array.Accelerate
import           Data.Map              (Map)

import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.RGB            as Color
import           Flowbox.Graphics.Color.RGBA           as Color
import           Flowbox.Graphics.Color.HSV            as Color
import           Flowbox.Graphics.Color.HSL            as Color
import           Flowbox.Graphics.Color.CMY            as Color
import           Flowbox.Graphics.Color.CMYK           as Color
import           Flowbox.Graphics.Color.YUV            as Color
import           Flowbox.Graphics.Color.YUV_HD         as Color
import           Flowbox.Graphics.Color.YUV.Conversion as Color
import           Flowbox.Graphics.Image                (Image)
import qualified Flowbox.Graphics.Image                as Image
import           Flowbox.Graphics.Image.Color.Helpers
import           Flowbox.Graphics.Image.Channel        (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel        as Channel
import           Flowbox.Graphics.Image.ImageRGB       (ImageRGB(..))
import           Flowbox.Graphics.Image.ImageRGBA      (ImageRGBA(..))
import           Flowbox.Graphics.Image.ImageHSV       (ImageHSV(..))
import           Flowbox.Graphics.Image.ImageHSL       (ImageHSL(..))
import           Flowbox.Graphics.Image.ImageCMY       (ImageCMY(..))
import           Flowbox.Graphics.Image.ImageCMYK      (ImageCMYK(..))
import           Flowbox.Graphics.Image.ImageYUV       (ImageYUV(..))
import           Flowbox.Graphics.Image.ImageYUV_HD    (ImageYUV_HD(..))
import           Flowbox.Prelude



class ColorSpaceAccYUV img where
    toYUV :: (Shape ix, Elt a, IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageYUV (ChannelAcc ix a))

instance ColorSpaceAccYUV ImageYUV where
    toYUV = return . id

instance ColorSpaceAccYUV ImageRGB where
    toYUV img@(ImageRGB chans) = threeWayConvert' Color.RGB ("rgb.r", "rgb.g", "rgb.b") img chans

instance ColorSpaceAccYUV ImageRGBA where
    toYUV img@(ImageRGBA chans) = convertFromFour ("yuv.y", "yuv.u", "yuv.v") ("rgba.r", "rgba.g", "rgba.b", "rgba.a") (convertFourToYUV Color.RGBA) ImageYUV img chans

instance ColorSpaceAccYUV ImageHSV where
    toYUV img@(ImageHSV chans) = threeWayConvert' Color.HSV ("hsv.h", "hsv.s", "hsv.v") img chans

instance ColorSpaceAccYUV ImageHSL where
    toYUV img@(ImageHSL chans) = threeWayConvert' Color.YUV ("hsl.h", "hsl.s", "hsl.l") img chans

instance ColorSpaceAccYUV ImageCMY where
    toYUV img@(ImageCMY chans) = threeWayConvert' Color.CMY ("cmy.c", "cmy.m", "cmy.y") img chans

instance ColorSpaceAccYUV ImageCMYK where
    toYUV img@(ImageCMYK chans) = convertFromFour ("yuv.y", "yuv.u", "yuv.v") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") (convertFourToYUV Color.CMYK) ImageYUV img chans

instance ColorSpaceAccYUV ImageYUV_HD where
    toYUV img@(ImageYUV_HD chans) = threeWayConvert' Color.YUV_HD ("yuv_hd.y", "yuv_hd.u", "yuv_hd.v") img chans


-- = Helpers

threeWayConvert' convertFrom = threeWayConvert ImageYUV ("yuv.y", "yuv.u", "yuv.v") (convertToYUV convertFrom)

convertFourToYUV :: (Elt a, IsFloating a, Unlift c (t, t1, t2, t3), Lift c2 (Exp a, Exp a, Exp a), ColorConvertAcc c1 YUV)
    => (t -> t1 -> t2 -> t3 -> c1 (Exp a)) -> c (Plain t, Plain t1, Plain t2, Plain t3)
    -> c2 (a, a, a)
convertFourToYUV fromSpace cmyk' = lift (h, s, v)
    where (c', m', y', k') = unlift cmyk'
          Color.YUV h s v = Color.toYUV $ fromSpace c' m' y' k'

convertToYUV :: (Elt a, IsFloating a, Unlift b (x, y, z), Lift c (Exp a, Exp a, Exp a), ColorConvertAcc c1 YUV)
    => (x -> y -> z -> c1 (Exp a)) -> b (Plain x, Plain y, Plain z) -> c (a, a, a)
convertToYUV fromSpace zxc' = lift (r, g, b)
    where (z', x', c') = unlift zxc'
          Color.YUV r g b = Color.toYUV $ fromSpace z' x' c'

convertCMYKtoYUV :: (Elt a, IsFloating a, Unlift c (Exp a, Exp a, Exp a, Exp a), Lift c1 (Exp a, Exp a, Exp a))
    => c (Plain (Exp a), Plain (Exp a), Plain (Exp a), Plain (Exp a)) -> c1 (a, a, a)
convertCMYKtoYUV cmyk' = lift (r, g, b)
    where (c', m', y', k') = unlift cmyk'
          Color.YUV r g b = Color.toYUV $ Color.CMYK c' m' y' k'
