---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.CMYK.Conversion where

import           Data.Array.Accelerate

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
import           Flowbox.Prelude



toCMYK :: (Elt a, IsFloating a, ColorConvertAcc c CMYK) => c (Exp a) -> CMYK (Exp a)
toCMYK = convertColorAcc

instance ColorConvertAcc CMYK CMYK where
    convertColorAcc = id

instance ColorConvertAcc RGB CMYK where
    convertColorAcc (RGB r' g' b') = CMYK c' m' y' k'
        where c'  = (1 - r' - k') / k''
              m'  = (1 - g' - k') / k''
              y'  = (1 - b' - k') / k''
              k'  = 1 - maxRGB
              k'' = 1 - k'
              maxRGB = max r' $ max g' b'

instance ColorConvertAcc RGBA CMYK where
    convertColorAcc = helperColorConverter toCMYK

instance ColorConvertAcc HSV CMYK where
    convertColorAcc = helperColorConverter toCMYK

instance ColorConvertAcc HSL CMYK where
    convertColorAcc = helperColorConverter toCMYK

instance ColorConvertAcc CMY CMYK where
    convertColorAcc = helperColorConverter toCMYK

instance ColorConvertAcc YUV CMYK where
    convertColorAcc = helperColorConverter toCMYK

instance ColorConvertAcc YUV_HD CMYK where
    convertColorAcc = helperColorConverter toCMYK
