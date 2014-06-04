---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Color.Convert.HSV (
    ColorSpaceAccHSV
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
import           Flowbox.Graphics.Color.HSV.Conversion as Color
import           Flowbox.Graphics.Channel              (ChannelAcc)
import qualified Flowbox.Graphics.Channel              as Channel
import           Flowbox.Graphics.Image                (Image)
import qualified Flowbox.Graphics.Image                as Image
import           Flowbox.Graphics.Image.Color.Helpers
import           Flowbox.Graphics.Image.Color.RGB      (ImageRGB(..))
import           Flowbox.Graphics.Image.Color.RGBA     (ImageRGBA(..))
import           Flowbox.Graphics.Image.Color.HSV      (ImageHSV(..))
import           Flowbox.Graphics.Image.Color.HSL      (ImageHSL(..))
import           Flowbox.Graphics.Image.Color.CMY      (ImageCMY(..))
import           Flowbox.Graphics.Image.Color.CMYK     (ImageCMYK(..))
import           Flowbox.Graphics.Image.Color.YUV      (ImageYUV(..))
import           Flowbox.Graphics.Image.Color.YUV_HD   (ImageYUV_HD(..))
import           Flowbox.Prelude



class ColorSpaceAccHSV img where
    toHSV :: (Shape ix, Elt a, IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageHSV (ChannelAcc ix a))

instance ColorSpaceAccHSV ImageHSV where
    toHSV = return . id

instance ColorSpaceAccHSV ImageRGB where
    toHSV img@(ImageRGB chans) = threeWayConvert' Color.RGB ("rgb.r", "rgb.g", "rgb.b") img chans

instance ColorSpaceAccHSV ImageRGBA where
    toHSV img@(ImageRGBA chans) = convertFromFour ("hsv.h", "hsv.s", "hsv.v") ("rgba.r", "rgba.g", "rgba.b", "rgba.a") (convertFourToHSV Color.RGBA) ImageHSV img chans

instance ColorSpaceAccHSV ImageHSL where
    toHSV img@(ImageHSL chans) = threeWayConvert' Color.HSL ("hsl.h", "hsl.s", "hsl.l") img chans

instance ColorSpaceAccHSV ImageCMY where
    toHSV img@(ImageCMY chans) = threeWayConvert' Color.CMY ("cmy.c", "cmy.m", "cmy.y") img chans

instance ColorSpaceAccHSV ImageCMYK where
    toHSV img@(ImageCMYK chans) = convertFromFour ("hsv.h", "hsv.s", "hsv.v") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") (convertFourToHSV Color.CMYK) ImageHSV img chans

instance ColorSpaceAccHSV ImageYUV where
    toHSV img@(ImageYUV chans) = threeWayConvert' Color.YUV ("yuv.y", "yuv.u", "yuv.v") img chans

instance ColorSpaceAccHSV ImageYUV_HD where
    toHSV img@(ImageYUV_HD chans) = threeWayConvert' Color.YUV_HD ("yuv_hd.y", "yuv_hd.u", "yuv_hd.v") img chans


-- = Helpers

threeWayConvert' convertFrom = threeWayConvert ImageHSV ("hsv.h", "hsv.s", "hsv.v") (convertToHSV convertFrom)

convertFourToHSV :: (Elt a, IsFloating a, Unlift c (t, t1, t2, t3), Lift c2 (Exp a, Exp a, Exp a), ColorConvertAcc c1 HSV)
    => (t -> t1 -> t2 -> t3 -> c1 (Exp a)) -> c (Plain t, Plain t1, Plain t2, Plain t3)
    -> c2 (a, a, a)
convertFourToHSV fromSpace cmyk' = lift (h, s, v)
    where (c', m', y', k') = unlift cmyk'
          Color.HSV h s v = Color.toHSV $ fromSpace c' m' y' k'

convertToHSV :: (Elt a, IsFloating a, Unlift b (x, y, z), Lift c (Exp a, Exp a, Exp a), ColorConvertAcc c1 HSV)
    => (x -> y -> z -> c1 (Exp a)) -> b (Plain x, Plain y, Plain z) -> c (a, a, a)
convertToHSV fromSpace zxc' = lift (r, g, b)
    where (z', x', c') = unlift zxc'
          Color.HSV r g b = Color.toHSV $ fromSpace z' x' c'

convertCMYKtoHSV :: (Elt a, IsFloating a, Unlift c (Exp a, Exp a, Exp a, Exp a), Lift c1 (Exp a, Exp a, Exp a))
    => c (Plain (Exp a), Plain (Exp a), Plain (Exp a), Plain (Exp a)) -> c1 (a, a, a)
convertCMYKtoHSV cmyk' = lift (r, g, b)
    where (c', m', y', k') = unlift cmyk'
          Color.HSV r g b = Color.toHSV $ Color.CMYK c' m' y' k'
