---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.RGBA.Conversion where

import           Data.Array.Accelerate

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
import           Flowbox.Prelude



toRGBA :: (Elt a, IsFloating a, ColorConvertAcc c RGBA) => c (Exp a) -> RGBA (Exp a)
toRGBA = convertColorAcc

instance ColorConvertAcc RGBA RGBA where
    convertColorAcc = id

instance ColorConvertAcc RGB RGBA where
    convertColorAcc (RGB r' g' b') = RGBA r' g' b' 1

instance ColorConvertAcc HSV RGBA where
    convertColorAcc = helperColorConverter toRGBA

instance ColorConvertAcc HSL RGBA where
    convertColorAcc = helperColorConverter toRGBA

instance ColorConvertAcc CMY RGBA where
    convertColorAcc = helperColorConverter toRGBA

instance ColorConvertAcc CMYK RGBA where
    convertColorAcc = helperColorConverter toRGBA

instance ColorConvertAcc YUV RGBA where
    convertColorAcc = helperColorConverter toRGBA

instance ColorConvertAcc YUV_HD RGBA where
    convertColorAcc = helperColorConverter toRGBA
