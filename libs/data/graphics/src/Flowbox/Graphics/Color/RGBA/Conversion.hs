---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.RGBA.Conversion where

import           Data.Array.Accelerate

import           Flowbox.Graphics.Color.CMY
import           Flowbox.Graphics.Color.CMYK
import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.Helpers
import           Flowbox.Graphics.Color.HSL
import           Flowbox.Graphics.Color.HSV
import           Flowbox.Graphics.Color.RGB
import           Flowbox.Graphics.Color.RGBA
import           Flowbox.Graphics.Color.YCbCr
import           Flowbox.Graphics.Color.YCbCr_HD
import           Flowbox.Prelude



toRGBA :: (Elt a, IsFloating a, ColorConvert c RGBA) => c (Exp a) -> RGBA (Exp a)
toRGBA = convertColor

instance ColorConvert RGBA RGBA where
    convertColor = id

instance ColorConvert RGB RGBA where
    convertColor (RGB r' g' b') = RGBA r' g' b' 1

instance ColorConvert HSV RGBA where
    convertColor = helperColorConverter toRGBA

instance ColorConvert HSL RGBA where
    convertColor = helperColorConverter toRGBA

instance ColorConvert CMY RGBA where
    convertColor = helperColorConverter toRGBA

instance ColorConvert CMYK RGBA where
    convertColor = helperColorConverter toRGBA

instance ColorConvert YCbCr RGBA where
    convertColor = helperColorConverter toRGBA

instance ColorConvert YCbCr_HD RGBA where
    convertColor = helperColorConverter toRGBA
