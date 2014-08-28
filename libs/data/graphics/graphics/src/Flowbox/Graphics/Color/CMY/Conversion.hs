---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.CMY.Conversion where

import           Data.Array.Accelerate

import           Flowbox.Graphics.Color.CMY
import           Flowbox.Graphics.Color.CMYK
import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.Helpers
import           Flowbox.Graphics.Color.HSL
import           Flowbox.Graphics.Color.HSV
import           Flowbox.Graphics.Color.RGB
import           Flowbox.Graphics.Color.RGBA
import           Flowbox.Graphics.Color.YUV
import           Flowbox.Graphics.Color.YUV_HD
import           Flowbox.Prelude



toCMY :: (Elt a, IsFloating a, ColorConvert c CMY) => c (Exp a) -> CMY (Exp a)
toCMY = convertColor

instance ColorConvert CMY CMY where
    convertColor = id

instance ColorConvert RGB CMY where
    convertColor (RGB r' g' b') = CMY c' m' y'
        where c' = 1 - r'
              m' = 1 - g'
              y' = 1 - b'

instance ColorConvert RGBA CMY where
    convertColor = helperColorConverter toCMY

instance ColorConvert HSV CMY where
    convertColor = helperColorConverter toCMY

instance ColorConvert HSL CMY where
    convertColor = helperColorConverter toCMY

instance ColorConvert CMYK CMY where
    convertColor = helperColorConverter toCMY

instance ColorConvert YUV CMY where
    convertColor = helperColorConverter toCMY

instance ColorConvert YUV_HD CMY where
    convertColor = helperColorConverter toCMY
