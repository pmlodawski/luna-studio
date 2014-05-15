---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Graphics.Image.Color.Convert where

import           Data.Map                          (Map)
import qualified Data.Array.Accelerate as A

import qualified Flowbox.Graphics.Color           as Color
import           Flowbox.Graphics.Image           (Image)
import qualified Flowbox.Graphics.Image           as Image
import           Flowbox.Graphics.Image.Channel   (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel   as Channel
import           Flowbox.Graphics.Image.ImageRGB  (ImageRGB(..))
import           Flowbox.Graphics.Image.ImageRGBA (ImageRGBA(..))
import           Flowbox.Graphics.Image.ImageHSV  (ImageHSV(..))
import           Flowbox.Graphics.Image.ImageHSL  (ImageHSL(..))
import           Flowbox.Graphics.Image.ImageCMY  (ImageCMY(..))
import           Flowbox.Graphics.Image.ImageCMYK (ImageCMYK(..))
import           Flowbox.Graphics.Image.ImageYUV  (ImageYUV(..))
import qualified Flowbox.Graphics.Utils           as U
import           Flowbox.Prelude

--class ColorSpaceRGB img where
--    toRGB :: img a -> Image.Result (ImageRGB a)

-- TODO: hide this (and som other shitty stuff) from exports

threeWayConvert :: (A.Shape ix, A.Elt a, A.Elt b, Image imgA (ChannelAcc ix a), Image imgB (ChannelAcc ix b))
    => (Channel.Name, Channel.Name, Channel.Name) -> (Channel.Name, Channel.Name, Channel.Name)
    -> (A.Exp (a, a, a) -> A.Exp (b, b, b))
    -> (t -> imgB (ChannelAcc ix b)) -> imgA (ChannelAcc ix a) -> t
    -> Either Image.Error (imgB (ChannelAcc ix b))
threeWayConvert (nameOutA, nameOutB, nameOutC) (nameInA, nameInB, nameInC) converter convertTo img chans = do
    chanInA <- Image.get nameInA img
    chanInB <- Image.get nameInB img
    chanInC <- Image.get nameInC img
    let chansIn = A.zip3 (Channel.accMatrix chanInA) (Channel.accMatrix chanInB) (Channel.accMatrix chanInC)
        chansOut = A.map converter chansIn
        chanOutA = Channel.Acc $ A.map U.fstTrio chansOut
        chanOutB = Channel.Acc $ A.map U.sndTrio chansOut
        chanOutC = Channel.Acc $ A.map U.trdTrio chansOut
    return $ Image.insert nameOutA chanOutA
           $ Image.insert nameOutB chanOutB
           $ Image.insert nameOutC chanOutC
           $ convertTo chans

convertFromFour :: (A.Shape ix, A.Elt a, A.Elt b, Image imgA (ChannelAcc ix a), Image imgB (ChannelAcc ix b))
    => (Channel.Name, Channel.Name, Channel.Name) -> (Channel.Name, Channel.Name, Channel.Name, Channel.Name)
    -> (A.Exp (a, a, a, a) -> A.Exp (b, b, b))
    -> (t -> imgB (ChannelAcc ix b)) -> imgA (ChannelAcc ix a) -> t
    -> Either Image.Error (imgB (ChannelAcc ix b))
convertFromFour (nameOutA, nameOutB, nameOutC) (nameInA, nameInB, nameInC, nameInD) converter convertTo img chans = do
    inA <- Image.get nameInA img
    inB <- Image.get nameInB img
    inC <- Image.get nameInC img
    inD <- Image.get nameInD img
    let chansIn = A.zip4 (Channel.accMatrix inA) (Channel.accMatrix inB) (Channel.accMatrix inC) (Channel.accMatrix inD)
        chansOut = A.map converter chansIn
        chanOutA = Channel.Acc $ A.map U.fstTrio chansOut
        chanOutB = Channel.Acc $ A.map U.sndTrio chansOut
        chanOutC = Channel.Acc $ A.map U.trdTrio chansOut
    return $ Image.insert nameOutA chanOutA
           $ Image.insert nameOutB chanOutB
           $ Image.insert nameOutC chanOutC
           $ convertTo chans


-- = Conversion to RGB

convertToRGB :: (A.Elt a, A.IsFloating a, A.Unlift b (x, y, z), A.Lift c (A.Exp a, A.Exp a, A.Exp a))
    => (x -> y -> z -> Color.ColorAcc a) -> b (A.Plain x, A.Plain y, A.Plain z) -> c (a, a, a)
convertToRGB fromSpace zxc' = A.lift (r, g, b)
    where (z', x', c') = A.unlift zxc'
          Color.RGB r g b = Color.toRGB $ fromSpace z' x' c'

convertCMYKtoRGB :: (A.Elt a, A.IsFloating a, A.Unlift c (A.Exp a, A.Exp a, A.Exp a, A.Exp a), A.Lift c1 (A.Exp a, A.Exp a, A.Exp a))
    => c (A.Plain (A.Exp a), A.Plain (A.Exp a), A.Plain (A.Exp a), A.Plain (A.Exp a)) -> c1 (a, a, a)
convertCMYKtoRGB cmyk' = A.lift (r, g, b)
    where (c', m', y', k') = A.unlift cmyk'
          Color.RGB r g b = Color.toRGB $ Color.CMYK c' m' y' k'

class ColorSpaceAccRGB img where
    toRGB :: (A.Shape ix, A.Elt a, A.IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageRGB (ChannelAcc ix a))

instance ColorSpaceAccRGB ImageRGB where
    toRGB = return . id

instance ColorSpaceAccRGB ImageRGBA where
    toRGB (ImageRGBA chans) = return $ ImageRGB chans

instance ColorSpaceAccRGB ImageHSV where
    toRGB img@(ImageHSV chans) = threeWayConvert ("rgb.r", "rgb.g", "rgb.b") ("hsv.h", "hsv.s", "hsv.v") (convertToRGB Color.HSV) ImageRGB img chans

instance ColorSpaceAccRGB ImageHSL where
    toRGB img@(ImageHSL chans) = threeWayConvert ("rgb.r", "rgb.g", "rgb.b") ("hsl.h", "hsl.s", "hsl.l") (convertToRGB Color.HSL) ImageRGB img chans

instance ColorSpaceAccRGB ImageCMY where
    toRGB img@(ImageCMY chans) = threeWayConvert ("rgb.r", "rgb.g", "rgb.b") ("cmy.c", "cmy.m", "cmy.y") (convertToRGB Color.CMY) ImageRGB img chans

instance ColorSpaceAccRGB ImageCMYK where
    toRGB img@(ImageCMYK chans) = convertFromFour ("rgb.r", "rgb.g", "rgb.b") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") convertCMYKtoRGB ImageRGB img chans

instance ColorSpaceAccRGB ImageYUV where
    toRGB img@(ImageYUV chans) = threeWayConvert ("rgb.r", "rgb.g", "rgb.b") ("yuv.y", "yuv.u", "yuv.v") (convertToRGB Color.YUV) ImageRGB img chans


-- = Conversion to RGBA

convertToRGBA ::(A.Shape ix, A.Elt e, A.Elt a, A.IsNum a, Image img (ChannelAcc ix e))
    => (Channel.Name, Channel.Name, Channel.Name)
    -> (A.Exp (e, e, e) -> A.Exp (a, a, a))
    -> img (ChannelAcc ix e) -> Map Channel.Name (ChannelAcc ix a)
    -> Either Image.Error (ImageRGBA (ChannelAcc ix a))
convertToRGBA namesIn@(nameInA, _, _) converter img chans = do
    result <- threeWayConvert ("rgba.r", "rgba.g", "rgba.b") namesIn converter ImageRGBA img chans
    testChan <- Image.get nameInA img
    return $ Image.insert "rgba.a" (Channel.fill (Channel.shape testChan) 1)
           $ result

class ColorSpaceAccRGBA img where
    toRGBA :: (A.Shape ix, A.Elt a, A.IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageRGBA (ChannelAcc ix a))

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
    toRGBA img@(ImageHSV chans) = convertToRGBA ("hsv.h", "hsv.s", "hsv.v") (convertToRGB Color.HSV) img chans

instance ColorSpaceAccRGBA ImageHSL where
    toRGBA img@(ImageHSL chans) = convertToRGBA ("hsl.h", "hsl.s", "hsl.l") (convertToRGB Color.HSL) img chans

instance ColorSpaceAccRGBA ImageCMY where
    toRGBA img@(ImageCMY chans) = convertToRGBA ("cmy.c", "cmy.m", "cmy.y") (convertToRGB Color.CMY) img chans

instance ColorSpaceAccRGBA ImageCMYK where
    toRGBA img@(ImageCMYK chans) = do
        result <- convertFromFour ("rgba.r", "rgba.g", "rgba.b") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") convertCMYKtoRGB ImageRGBA img chans
        testChan <- Image.get "rgba.r" result
        return $ Image.insert "rgba.a" (Channel.fill (Channel.shape testChan) 1) result

instance ColorSpaceAccRGBA ImageYUV where
    toRGBA img@(ImageYUV chans) = convertToRGBA ("yuv.y", "yuv.u", "yuv.v") (convertToRGB Color.YUV) img chans


-- = Conversion to HSV

convertToHSV :: (A.Elt a, A.IsFloating a, A.Unlift b (x, y, z), A.Lift c (A.Exp a, A.Exp a, A.Exp a))
    => (x -> y -> z -> Color.ColorAcc a) -> b (A.Plain x, A.Plain y, A.Plain z) -> c (a, a, a)
convertToHSV fromSpace zxc' = A.lift (h, s, v)
    where (z', x', c') = A.unlift zxc'
          Color.HSV h s v = Color.toHSV $ fromSpace z' x' c'

convertFourToHSV :: (A.Elt a, A.IsFloating a, A.Unlift c (t, t1, t2, t3), A.Lift c1 (A.Exp a, A.Exp a, A.Exp a))
    => (t -> t1 -> t2 -> t3 -> Color.ColorAcc a) -> c (A.Plain t, A.Plain t1, A.Plain t2, A.Plain t3)
    -> c1 (a, a, a)
convertFourToHSV fromSpace cmyk' = A.lift (h, s, v)
    where (c', m', y', k') = A.unlift cmyk'
          Color.HSV h s v = Color.toHSV $ fromSpace c' m' y' k'

class ColorSpaceAccHSV img where
    toHSV :: (A.Shape ix, A.Elt a, A.IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageHSV (ChannelAcc ix a))

instance ColorSpaceAccHSV ImageHSV where
    toHSV = return . id

instance ColorSpaceAccHSV ImageRGB where
    toHSV img@(ImageRGB chans) = threeWayConvert ("hsv.h", "hsv.s", "hsv.v") ("rgb.r", "rgb.g", "rgb.b") (convertToHSV Color.RGB) ImageHSV img chans


instance ColorSpaceAccHSV ImageRGBA where
    toHSV img@(ImageRGBA chans) = convertFromFour ("hsv.h", "hsv.s", "hsv.v") ("rgba.r", "rgba.g", "rgba.b", "rgba.a") (convertFourToHSV Color.RGBA) ImageHSV img chans

instance ColorSpaceAccHSV ImageHSL where
    toHSV img@(ImageHSL chans) = threeWayConvert ("hsv.h", "hsv.s", "hsv.v") ("hsl.h", "hsl.s", "hsl.l") (convertToHSV Color.HSL) ImageHSV img chans

instance ColorSpaceAccHSV ImageCMY where
    toHSV img@(ImageCMY chans) = threeWayConvert ("hsv.h", "hsv.s", "hsv.v") ("cmy.c", "cmy.m", "cmy.y") (convertToHSV Color.CMY) ImageHSV img chans

instance ColorSpaceAccHSV ImageCMYK where
    toHSV img@(ImageCMYK chans) = convertFromFour ("hsv.h", "hsv.s", "hsv.v") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") (convertFourToHSV Color.CMYK) ImageHSV img chans

instance ColorSpaceAccHSV ImageYUV where
    toHSV img@(ImageYUV chans) = threeWayConvert ("hsv.h", "hsv.s", "hsv.v") ("yuv.y", "yuv.u", "yuv.v") (convertToHSV Color.YUV) ImageHSV img chans


-- = Conversion to HSL

convertToHSL :: (A.Elt a, A.IsFloating a, A.Unlift b (x, y, z), A.Lift c (A.Exp a, A.Exp a, A.Exp a))
    => (x -> y -> z -> Color.ColorAcc a) -> b (A.Plain x, A.Plain y, A.Plain z) -> c (a, a, a)
convertToHSL fromSpace zxc' = A.lift (h, s, v)
    where (z', x', c') = A.unlift zxc'
          Color.HSL h s v = Color.toHSL $ fromSpace z' x' c'

convertFourToHSL :: (A.Elt a, A.IsFloating a, A.Unlift c (t, t1, t2, t3), A.Lift c1 (A.Exp a, A.Exp a, A.Exp a))
    => (t -> t1 -> t2 -> t3 -> Color.ColorAcc a) -> c (A.Plain t, A.Plain t1, A.Plain t2, A.Plain t3)
    -> c1 (a, a, a)
convertFourToHSL fromSpace cmyk' = A.lift (h, s, l)
    where (c', m', y', k') = A.unlift cmyk'
          Color.HSL h s l = Color.toHSL $ fromSpace c' m' y' k'

class ColorSpaceAccHSL img where
    toHSL :: (A.Shape ix, A.Elt a, A.IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageHSL (ChannelAcc ix a))

instance ColorSpaceAccHSL ImageHSL where
    toHSL = return . id

instance ColorSpaceAccHSL ImageRGB where
    toHSL img@(ImageRGB chans) = threeWayConvert ("hsl.h", "hsl.s", "hsl.l") ("rgb.r", "rgb.g", "rgb.b") (convertToHSL Color.RGB) ImageHSL img chans

instance ColorSpaceAccHSL ImageRGBA where
    toHSL img@(ImageRGBA chans) = convertFromFour ("hsl.h", "hsl.s", "hsl.l") ("rgba.r", "rgba.g", "rgba.b", "rgba.a") (convertFourToHSL Color.RGBA) ImageHSL img chans

instance ColorSpaceAccHSL ImageHSV where
    toHSL img@(ImageHSV chans) = threeWayConvert ("hsl.h", "hsl.s", "hsl.l") ("hsv.h", "hsv.s", "hsv.v") (convertToHSL Color.HSV) ImageHSL img chans

instance ColorSpaceAccHSL ImageCMY where
    toHSL img@(ImageCMY chans) = threeWayConvert ("hsl.h", "hsl.s", "hsl.l") ("cmy.c", "cmy.m", "cmy.y") (convertToHSL Color.CMY) ImageHSL img chans

instance ColorSpaceAccHSL ImageCMYK where
    toHSL img@(ImageCMYK chans) = convertFromFour ("hsl.h", "hsl.s", "hsl.l") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") (convertFourToHSL Color.CMYK) ImageHSL img chans

instance ColorSpaceAccHSL ImageYUV where
    toHSL img@(ImageYUV chans) = threeWayConvert ("hsl.h", "hsl.s", "hsl.l") ("yuv.y", "yuv.u", "yuv.v") (convertToHSL Color.YUV) ImageHSL img chans


-- = Conversion to CMY

convertToCMY :: (A.Elt a, A.IsFloating a, A.Unlift b (x, y, z), A.Lift c (A.Exp a, A.Exp a, A.Exp a))
    => (x -> y -> z -> Color.ColorAcc a) -> b (A.Plain x, A.Plain y, A.Plain z) -> c (a, a, a)
convertToCMY fromSpace zxc' = A.lift (h, s, v)
    where (z', x', c') = A.unlift zxc'
          Color.CMY h s v = Color.toCMY $ fromSpace z' x' c'

convertFourToCMY :: (A.Elt a, A.IsFloating a, A.Unlift c (t, t1, t2, t3), A.Lift c1 (A.Exp a, A.Exp a, A.Exp a))
    => (t -> t1 -> t2 -> t3 -> Color.ColorAcc a) -> c (A.Plain t, A.Plain t1, A.Plain t2, A.Plain t3)
    -> c1 (a, a, a)
convertFourToCMY fromSpace cmyk' = A.lift (y, u, v)
    where (c', m', y', k') = A.unlift cmyk'
          Color.CMY y u v = Color.toCMY $ fromSpace c' m' y' k'

class ColorSpaceAccCMY img where
    toCMY :: (A.Shape ix, A.Elt a, A.IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageCMY (ChannelAcc ix a))

instance ColorSpaceAccCMY ImageCMY where
    toCMY = return . id

instance ColorSpaceAccCMY ImageRGB where
    toCMY img@(ImageRGB chans) = threeWayConvert ("cmy.c", "cmy.m", "cmy.y") ("rgb.r", "rgb.g", "rgb.b") (convertToCMY Color.RGB) ImageCMY img chans

instance ColorSpaceAccCMY ImageRGBA where
    toCMY img@(ImageRGBA chans) = convertFromFour ("cmy.c", "cmy.m", "cmy.y") ("rgba.r", "rgba.g", "rgba.b", "rgba.a") (convertFourToCMY Color.RGBA) ImageCMY img chans

instance ColorSpaceAccCMY ImageHSV where
    toCMY img@(ImageHSV chans) = threeWayConvert ("cmy.c", "cmy.m", "cmy.y") ("hsv.h", "hsv.s", "hsv.v") (convertToCMY Color.HSV) ImageCMY img chans

instance ColorSpaceAccCMY ImageHSL where
    toCMY img@(ImageHSL chans) = threeWayConvert ("cmy.c", "cmy.m", "cmy.y") ("hsl.h", "hsl.s", "hsl.l") (convertToCMY Color.HSL) ImageCMY img chans

instance ColorSpaceAccCMY ImageCMYK where
    toCMY img@(ImageCMYK chans) = convertFromFour ("cmy.c", "cmy.m", "cmy.y") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") (convertFourToCMY Color.CMYK) ImageCMY img chans

instance ColorSpaceAccCMY ImageYUV where
    toCMY img@(ImageYUV chans) = threeWayConvert ("cmy.c", "cmy.m", "cmy.y") ("yuv.y", "yuv.u", "yuv.v") (convertToCMY Color.YUV) ImageCMY img chans


-- = Conversion to CMYK

-- TODO

-- = Conversion to YUV

convertToYUV :: (A.Elt a, A.IsFloating a, A.Unlift b (x, y, z), A.Lift c (A.Exp a, A.Exp a, A.Exp a))
    => (x -> y -> z -> Color.ColorAcc a) -> b (A.Plain x, A.Plain y, A.Plain z) -> c (a, a, a)
convertToYUV fromSpace zxc' = A.lift (h, s, v)
    where (z', x', c') = A.unlift zxc'
          Color.YUV h s v = Color.toYUV $ fromSpace z' x' c'

convertFourToYUV :: (A.Elt a, A.IsFloating a, A.Unlift c (t, t1, t2, t3), A.Lift c1 (A.Exp a, A.Exp a, A.Exp a))
    => (t -> t1 -> t2 -> t3 -> Color.ColorAcc a) -> c (A.Plain t, A.Plain t1, A.Plain t2, A.Plain t3)
    -> c1 (a, a, a)
convertFourToYUV fromSpace cmyk' = A.lift (y, u, v)
    where (c', m', y', k') = A.unlift cmyk'
          Color.YUV y u v = Color.toYUV $ fromSpace c' m' y' k'

class ColorSpaceAccYUV img where
    toYUV :: (A.Shape ix, A.Elt a, A.IsFloating a) => img (ChannelAcc ix a) -> Image.Result (ImageYUV (ChannelAcc ix a))

instance ColorSpaceAccYUV ImageYUV where
    toYUV = return . id

instance ColorSpaceAccYUV ImageRGB where
    toYUV img@(ImageRGB chans) = threeWayConvert ("yuv.y", "yuv.u", "yuv.v") ("rgb.r", "rgb.g", "rgb.b") (convertToYUV Color.RGB) ImageYUV img chans

instance ColorSpaceAccYUV ImageRGBA where
    toYUV img@(ImageRGBA chans) = convertFromFour ("yuv.y", "yuv.u", "yuv.v") ("rgba.r", "rgba.g", "rgba.b", "rgba.a") (convertFourToYUV Color.RGBA) ImageYUV img chans

instance ColorSpaceAccYUV ImageHSV where
    toYUV img@(ImageHSV chans) = threeWayConvert ("yuv.y", "yuv.u", "yuv.v") ("hsv.h", "hsv.s", "hsv.v") (convertToYUV Color.HSV) ImageYUV img chans

instance ColorSpaceAccYUV ImageHSL where
    toYUV img@(ImageHSL chans) = threeWayConvert ("yuv.y", "yuv.u", "yuv.v") ("hsl.h", "hsl.s", "hsl.l") (convertToYUV Color.HSL) ImageYUV img chans

instance ColorSpaceAccYUV ImageCMYK where
    toYUV img@(ImageCMYK chans) = convertFromFour ("yuv.y", "yuv.u", "yuv.v") ("cmyk.c", "cmyk.m", "cmyk.y", "cmyk.k") (convertFourToYUV Color.CMYK) ImageYUV img chans

instance ColorSpaceAccYUV ImageCMY where
    toYUV img@(ImageCMY chans) = threeWayConvert ("yuv.y", "yuv.u", "yuv.v") ("cmy.c", "cmy.m", "cmy.y") (convertToYUV Color.CMY) ImageYUV img chans
