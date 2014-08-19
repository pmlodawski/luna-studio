---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.HSV.Conversion where

import           Data.Array.Accelerate as A

import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.Helpers
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



toHSV :: (Elt a, IsFloating a, ColorConvertAcc c HSV) => c (Exp a) -> HSV (Exp a)
toHSV = convertColorAcc

instance ColorConvertAcc HSV HSV where
    convertColorAcc = id

instance ColorConvertAcc RGB HSV where
    convertColorAcc (RGB r' g' b') = HSV h'' s' v'
        where h'' = (h' >* 0 A.? (h' , h' + 6)) / 6
              h' = cond (delta ==* 0) 0
                 $ cond (r' ==* maxRGB) (((g' - b') / delta) `nonIntRem` 6)
                 $ cond (g' ==* maxRGB) ((b' - r') / delta + 2)
                 $ (r'-g') / delta + 4
              s' = delta ==* 0 A.? (0, delta / maxRGB)
              v' = maxRGB
              minRGB = min r' $ min g' b'
              maxRGB = max r' $ max g' b'
              delta = maxRGB - minRGB

instance ColorConvertAcc RGBA HSV where
    convertColorAcc = helperColorConverter toHSV

instance ColorConvertAcc HSL HSV where
    convertColorAcc = helperColorConverter toHSV

instance ColorConvertAcc CMY HSV where
    convertColorAcc = helperColorConverter toHSV

instance ColorConvertAcc CMYK HSV where
    convertColorAcc = helperColorConverter toHSV

instance ColorConvertAcc YUV HSV where
    convertColorAcc = helperColorConverter toHSV

instance ColorConvertAcc YUV_HD HSV where
    convertColorAcc = helperColorConverter toHSV
