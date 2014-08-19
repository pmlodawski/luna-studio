---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Color.Convert.RGBA (
    ColorSpaceAccRGBA
) where

import           Data.Array.Accelerate
import           Data.Map              (Map)

import           Flowbox.Graphics.Image.Color.Convert.RGB
import           Flowbox.Graphics.Color.HSV             as Color
import           Flowbox.Graphics.Color.HSL             as Color
import           Flowbox.Graphics.Color.CMY             as Color
import           Flowbox.Graphics.Color.YUV             as Color
import           Flowbox.Graphics.Color.YUV_HD          as Color
import           Flowbox.Graphics.Channel              (ChannelAcc)
import qualified Flowbox.Graphics.Channel              as Channel
import           Flowbox.Graphics.Image                 (Image)
import qualified Flowbox.Graphics.Image                 as Image
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



class ColorSpaceAccRGBA img where
    toRGBA :: (Shape ix, Elt a, IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageRGBA (ChannelAcc ix a))

instance ColorSpaceAccRGBA ImageRGBA where
    toRGBA = return . id

instance ColorSpaceAccRGBA ImageRGB where
    toRGBA img@(ImageRGB chans) = do
        chanR <- Image.get "rgb.r" img
        chanG <- Image.get "rgb.g" img
        chanB <- Image.get "rgb.b" img
        return $ Image.insert "rgba.r" chanR
               $ Image.insert "rgba.g" chanG
               $ Image.insert "rgba.b" chanB
               $ Image.insert "rgba.a" (Channel.fill (Channel.shape chanR) 1)
               $ ImageRGBA chans

instance ColorSpaceAccRGBA ImageHSV where
    toRGBA img@(ImageHSV chans) = convertToRGBA Color.HSV ("hsv.h", "hsv.s", "hsv.v") img chans

instance ColorSpaceAccRGBA ImageHSL where
    toRGBA img@(ImageHSL chans) = convertToRGBA Color.HSL ("hsl.h", "hsl.s", "hsl.l") img chans

instance ColorSpaceAccRGBA ImageCMY where
    toRGBA img@(ImageCMY chans) = convertToRGBA Color.CMY ("cmy.c", "cmy.m", "cmy.y") img chans

instance ColorSpaceAccRGBA ImageCMYK where
    toRGBA img@(ImageCMYK chans) = do
        result <- convertFromFour ("rgba.r", "rgba.g", "rgba.b") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") convertCMYKtoRGB ImageRGBA img chans
        testChan <- Image.get "rgba.r" result
        return $ Image.insert "rgba.a" (Channel.fill (Channel.shape testChan) 1) result

instance ColorSpaceAccRGBA ImageYUV where
    toRGBA img@(ImageYUV chans) = convertToRGBA Color.YUV ("yuv.y", "yuv.u", "yuv.v") img chans

instance ColorSpaceAccRGBA ImageYUV_HD where
    toRGBA img@(ImageYUV_HD chans) = convertToRGBA Color.YUV_HD ("yuv_hd.y", "yuv_hd.u", "yuv_hd.v") img chans


-- = Helpers

convertToRGBA convertFrom namesIn@(nameInA, _, _) img chans = do
    result <- threeWayConvert ImageRGBA ("rgba.r", "rgba.g", "rgba.b") (convertToRGB convertFrom) namesIn img chans
    testChan <- Image.get nameInA img
    return $ Image.insert "rgba.a" (Channel.fill (Channel.shape testChan) 1)
           $ result
