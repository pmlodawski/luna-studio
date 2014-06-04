---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
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



toCMYK :: (Elt a, IsFloating a, ColorConvert c CMYK) => c (Exp a) -> CMYK (Exp a)
toCMYK = convertColor

instance ColorConvert CMYK CMYK where
    convertColor = id

instance ColorConvert RGB CMYK where
    convertColor (RGB r' g' b') = CMYK c' m' y' k'
        where c'  = (1 - r' - k') / k''
              m'  = (1 - g' - k') / k''
              y'  = (1 - b' - k') / k''
              k'  = 1 - maxRGB
              k'' = 1 - k'
              maxRGB = max r' $ max g' b'

instance ColorConvert RGBA CMYK where
    convertColor = helperColorConverter toCMYK

instance ColorConvert HSV CMYK where
    convertColor = helperColorConverter toCMYK

instance ColorConvert HSL CMYK where
    convertColor = helperColorConverter toCMYK

instance ColorConvert CMY CMYK where
    convertColor = helperColorConverter toCMYK

instance ColorConvert YUV CMYK where
    convertColor = helperColorConverter toCMYK

instance ColorConvert YUV_HD CMYK where
    convertColor = helperColorConverter toCMYK
