---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Color.Convert.HSL (
    ColorSpaceAccHSL
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
import           Flowbox.Graphics.Color.HSL.Conversion as Color
import           Flowbox.Graphics.Image                (Image)
import qualified Flowbox.Graphics.Image                as Image
import           Flowbox.Graphics.Image.Color.Helpers
import           Flowbox.Graphics.Image.Channel        (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel        as Channel
import           Flowbox.Graphics.Image.Color.RGB      (ImageRGB(..))
import           Flowbox.Graphics.Image.Color.RGBA     (ImageRGBA(..))
import           Flowbox.Graphics.Image.Color.HSV      (ImageHSV(..))
import           Flowbox.Graphics.Image.Color.HSL      (ImageHSL(..))
import           Flowbox.Graphics.Image.Color.CMY      (ImageCMY(..))
import           Flowbox.Graphics.Image.Color.CMYK     (ImageCMYK(..))
import           Flowbox.Graphics.Image.Color.YUV      (ImageYUV(..))
import           Flowbox.Graphics.Image.Color.YUV_HD   (ImageYUV_HD(..))
import           Flowbox.Prelude



class ColorSpaceAccHSL img where
    toHSL :: (Shape ix, Elt a, IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageHSL (ChannelAcc ix a))

instance ColorSpaceAccHSL ImageHSL where
    toHSL = return . id

instance ColorSpaceAccHSL ImageRGB where
    toHSL img@(ImageRGB chans) = threeWayConvert' Color.RGB ("rgb.r", "rgb.g", "rgb.b") img chans

instance ColorSpaceAccHSL ImageRGBA where
    toHSL img@(ImageRGBA chans) = convertFromFour ("hsl.h", "hsl.s", "hsl.l") ("rgba.r", "rgba.g", "rgba.b", "rgba.a") (convertFourToHSL Color.RGBA) ImageHSL img chans

instance ColorSpaceAccHSL ImageHSV where
    toHSL img@(ImageHSV chans) = threeWayConvert' Color.HSV ("hsv.h", "hsv.s", "hsv.v") img chans

instance ColorSpaceAccHSL ImageCMY where
    toHSL img@(ImageCMY chans) = threeWayConvert' Color.CMY ("cmy.c", "cmy.m", "cmy.y") img chans

instance ColorSpaceAccHSL ImageCMYK where
    toHSL img@(ImageCMYK chans) = convertFromFour ("hsl.h", "hsl.s", "hsl.l") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") (convertFourToHSL Color.CMYK) ImageHSL img chans

instance ColorSpaceAccHSL ImageYUV where
    toHSL img@(ImageYUV chans) = threeWayConvert' Color.YUV ("yuv.y", "yuv.u", "yuv.v") img chans

instance ColorSpaceAccHSL ImageYUV_HD where
    toHSL img@(ImageYUV_HD chans) = threeWayConvert' Color.YUV_HD ("yuv_hd.y", "yuv_hd.u", "yuv_hd.v") img chans


-- = Helpers

threeWayConvert' convertFrom = threeWayConvert ImageHSL ("hsl.h", "hsl.s", "hsl.l") (convertToHSL convertFrom)

convertFourToHSL :: (Elt a, IsFloating a, Unlift c (t, t1, t2, t3), Lift c2 (Exp a, Exp a, Exp a), ColorConvertAcc c1 HSL)
    => (t -> t1 -> t2 -> t3 -> c1 (Exp a)) -> c (Plain t, Plain t1, Plain t2, Plain t3)
    -> c2 (a, a, a)
convertFourToHSL fromSpace cmyk' = lift (h, s, v)
    where (c', m', y', k') = unlift cmyk'
          Color.HSL h s v = Color.toHSL $ fromSpace c' m' y' k'

convertToHSL :: (Elt a, IsFloating a, Unlift b (x, y, z), Lift c (Exp a, Exp a, Exp a), ColorConvertAcc c1 HSL)
    => (x -> y -> z -> c1 (Exp a)) -> b (Plain x, Plain y, Plain z) -> c (a, a, a)
convertToHSL fromSpace zxc' = lift (r, g, b)
    where (z', x', c') = unlift zxc'
          Color.HSL r g b = Color.toHSL $ fromSpace z' x' c'

convertCMYKtoHSL :: (Elt a, IsFloating a, Unlift c (Exp a, Exp a, Exp a, Exp a), Lift c1 (Exp a, Exp a, Exp a))
    => c (Plain (Exp a), Plain (Exp a), Plain (Exp a), Plain (Exp a)) -> c1 (a, a, a)
convertCMYKtoHSL cmyk' = lift (r, g, b)
    where (c', m', y', k') = unlift cmyk'
          Color.HSL r g b = Color.toHSL $ Color.CMYK c' m' y' k'
