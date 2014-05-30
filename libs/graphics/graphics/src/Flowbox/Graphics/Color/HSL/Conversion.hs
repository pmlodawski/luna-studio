---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.HSL.Conversion where

import           Data.Array.Accelerate as A

import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.Helpers
import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.RGB
import           Flowbox.Graphics.Color.RGBA
import           Flowbox.Graphics.Color.HSV
import           Flowbox.Graphics.Color.HSL
import           Flowbox.Graphics.Color.CMY
import           Flowbox.Graphics.Color.CMYK
import           Flowbox.Graphics.Color.YUV
import           Flowbox.Graphics.Color.YUV_HD
import           Flowbox.Graphics.Utils
import           Flowbox.Prelude



toHSL :: (Elt a, IsFloating a, ColorConvertAcc c HSL) => c (Exp a) -> HSL (Exp a)
toHSL = convertColorAcc

instance ColorConvertAcc HSL HSL where
    convertColorAcc = id

instance ColorConvertAcc RGB HSL where
    convertColorAcc (RGB r' g' b') = HSL h'' s' l'
        where h'' = (h' >* 0 A.? (h' , h' + 6)) / 6
              h' = delta ==* 0 A.? (0,
                    r' ==* maxRGB A.? (((g' - b') / delta) `nonIntRem` 6,
                    g' ==* maxRGB A.? ((b' - r') / delta + 2,
                    (r'-g') / delta + 4
                  )))
              s' = delta ==* 0 A.? (0, delta / (1 - abs(2 * l' - 1)))
              l' = (maxRGB + minRGB) / 2
              minRGB = min r' $ min g' b'
              maxRGB = max r' $ max g' b'
              delta = maxRGB - minRGB

instance ColorConvertAcc RGBA HSL where
    convertColorAcc = helperColorConverter toHSL

instance ColorConvertAcc HSV HSL where
    convertColorAcc = helperColorConverter toHSL

instance ColorConvertAcc CMY HSL where
    convertColorAcc = helperColorConverter toHSL

instance ColorConvertAcc CMYK HSL where
    convertColorAcc = helperColorConverter toHSL

instance ColorConvertAcc YUV HSL where
    convertColorAcc = helperColorConverter toHSL

instance ColorConvertAcc YUV_HD HSL where
    convertColorAcc = helperColorConverter toHSL
