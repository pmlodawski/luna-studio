---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Color.Convert.CMY (
    ColorSpaceAccCMY
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
import           Flowbox.Graphics.Color.CMY.Conversion as Color
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



class ColorSpaceAccCMY img where
    toCMY :: (Shape ix, Elt a, IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageCMY (ChannelAcc ix a))

instance ColorSpaceAccCMY ImageCMY where
    toCMY = return . id

instance ColorSpaceAccCMY ImageRGB where
    toCMY img@(ImageRGB chans) = threeWayConvert' Color.RGB ("rgb.r", "rgb.g", "rgb.b") img chans

instance ColorSpaceAccCMY ImageRGBA where
    toCMY img@(ImageRGBA chans) = convertFromFour ("cmy.c", "cmy.m", "cmy.y") ("rgba.r", "rgba.g", "rgba.b", "rgba.a") (convertFourToCMY Color.RGBA) ImageCMY img chans

instance ColorSpaceAccCMY ImageHSV where
    toCMY img@(ImageHSV chans) = threeWayConvert' Color.HSV ("hsv.h", "hsv.s", "hsv.v") img chans

instance ColorSpaceAccCMY ImageHSL where
    toCMY img@(ImageHSL chans) = threeWayConvert' Color.CMY ("hsl.h", "hsl.s", "hsl.l") img chans

instance ColorSpaceAccCMY ImageCMYK where
    toCMY img@(ImageCMYK chans) = convertFromFour ("cmy.c", "cmy.m", "cmy.y") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") (convertFourToCMY Color.CMYK) ImageCMY img chans

instance ColorSpaceAccCMY ImageYUV where
    toCMY img@(ImageYUV chans) = threeWayConvert' Color.YUV ("yuv.y", "yuv.u", "yuv.v") img chans

instance ColorSpaceAccCMY ImageYUV_HD where
    toCMY img@(ImageYUV_HD chans) = threeWayConvert' Color.YUV_HD ("yuv_hd.y", "yuv_hd.u", "yuv_hd.v") img chans


-- = Helpers

threeWayConvert' convertFrom = threeWayConvert ImageCMY ("cmy.c", "cmy.m", "cmy.y") (convertToCMY convertFrom)

convertFourToCMY :: (Elt a, IsFloating a, Unlift c (t, t1, t2, t3), Lift c2 (Exp a, Exp a, Exp a), ColorConvertAcc c1 CMY)
    => (t -> t1 -> t2 -> t3 -> c1 (Exp a)) -> c (Plain t, Plain t1, Plain t2, Plain t3)
    -> c2 (a, a, a)
convertFourToCMY fromSpace cmyk' = lift (h, s, v)
    where (c', m', y', k') = unlift cmyk'
          Color.CMY h s v = Color.toCMY $ fromSpace c' m' y' k'

convertToCMY :: (Elt a, IsFloating a, Unlift b (x, y, z), Lift c (Exp a, Exp a, Exp a), ColorConvertAcc c1 CMY)
    => (x -> y -> z -> c1 (Exp a)) -> b (Plain x, Plain y, Plain z) -> c (a, a, a)
convertToCMY fromSpace zxc' = lift (r, g, b)
    where (z', x', c') = unlift zxc'
          Color.CMY r g b = Color.toCMY $ fromSpace z' x' c'

convertCMYKtoCMY :: (Elt a, IsFloating a, Unlift c (Exp a, Exp a, Exp a, Exp a), Lift c1 (Exp a, Exp a, Exp a))
    => c (Plain (Exp a), Plain (Exp a), Plain (Exp a), Plain (Exp a)) -> c1 (a, a, a)
convertCMYKtoCMY cmyk' = lift (r, g, b)
    where (c', m', y', k') = unlift cmyk'
          Color.CMY r g b = Color.toCMY $ Color.CMYK c' m' y' k'
