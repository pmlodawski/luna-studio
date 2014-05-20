---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Color.Convert.RGB (
    ColorSpaceAccRGB,
    convertToRGB,
    convertCMYKtoRGB
) where

import           Data.Array.Accelerate
import           Data.Map              (Map)

import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.RGB            as Color
import           Flowbox.Graphics.Color.HSV            as Color
import           Flowbox.Graphics.Color.HSL            as Color
import           Flowbox.Graphics.Color.CMY            as Color
import           Flowbox.Graphics.Color.CMYK           as Color
import           Flowbox.Graphics.Color.YUV            as Color
import           Flowbox.Graphics.Color.YUV_HD         as Color
import           Flowbox.Graphics.Color.RGB.Conversion as Color
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



class ColorSpaceAccRGB img where
    toRGB :: (Shape ix, Elt a, IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageRGB (ChannelAcc ix a))

instance ColorSpaceAccRGB ImageRGB where
    toRGB = return . id

instance ColorSpaceAccRGB ImageRGBA where
    toRGB (ImageRGBA chans) = return $ ImageRGB chans

instance ColorSpaceAccRGB ImageHSV where
    toRGB img@(ImageHSV chans) = threeWayConvert' Color.HSV ("hsv.h", "hsv.s", "hsv.v") img chans

instance ColorSpaceAccRGB ImageHSL where
    toRGB img@(ImageHSL chans) = threeWayConvert' Color.HSL ("hsl.h", "hsl.s", "hsl.l") img chans

instance ColorSpaceAccRGB ImageCMY where
    toRGB img@(ImageCMY chans) = threeWayConvert' Color.CMY ("cmy.c", "cmy.m", "cmy.y") img chans

instance ColorSpaceAccRGB ImageCMYK where
    toRGB img@(ImageCMYK chans) = convertFromFour ("rgb.r", "rgb.g", "rgb.b") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") convertCMYKtoRGB ImageRGB img chans

instance ColorSpaceAccRGB ImageYUV where
    toRGB img@(ImageYUV chans) = threeWayConvert' Color.YUV ("yuv.y", "yuv.u", "yuv.v") img chans

instance ColorSpaceAccRGB ImageYUV_HD where
    toRGB img@(ImageYUV_HD chans) = threeWayConvert' Color.YUV_HD ("yuv_hd.y", "yuv_hd.u", "yuv_hd.v") img chans


-- = Helpers

threeWayConvert' convertFrom = threeWayConvert ImageRGB ("rgb.r", "rgb.g", "rgb.b") (convertToRGB convertFrom)

convertToRGB :: (Elt a, IsFloating a, Unlift b (x, y, z), Lift c (Exp a, Exp a, Exp a), ColorConvertAcc c1 RGB)
    => (x -> y -> z -> c1 (Exp a)) -> b (Plain x, Plain y, Plain z) -> c (a, a, a)
convertToRGB fromSpace zxc' = lift (r, g, b)
    where (z', x', c') = unlift zxc'
          Color.RGB r g b = Color.toRGB $ fromSpace z' x' c'

convertCMYKtoRGB :: (Elt a, IsFloating a, Unlift c (Exp a, Exp a, Exp a, Exp a), Lift c1 (Exp a, Exp a, Exp a))
    => c (Plain (Exp a), Plain (Exp a), Plain (Exp a), Plain (Exp a)) -> c1 (a, a, a)
convertCMYKtoRGB cmyk' = lift (r, g, b)
    where (c', m', y', k') = unlift cmyk'
          Color.RGB r g b = Color.toRGB $ Color.CMYK c' m' y' k'
