---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.CMY.Conversion where

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



toCMY :: (Elt a, IsFloating a, ColorConvertAcc c CMY) => c (Exp a) -> CMY (Exp a)
toCMY = convertColorAcc

instance ColorConvertAcc CMY CMY where
    convertColorAcc = id

instance ColorConvertAcc RGB CMY where
    convertColorAcc (RGB r' g' b') = CMY c' m' y'
        where c' = 1 - r'
              m' = 1 - g'
              y' = 1 - b'

instance ColorConvertAcc RGBA CMY where
    convertColorAcc = helperColorConverter toCMY

instance ColorConvertAcc HSV CMY where
    convertColorAcc = helperColorConverter toCMY

instance ColorConvertAcc HSL CMY where
    convertColorAcc = helperColorConverter toCMY

instance ColorConvertAcc CMYK CMY where
    convertColorAcc = helperColorConverter toCMY

instance ColorConvertAcc YUV CMY where
    convertColorAcc = helperColorConverter toCMY

instance ColorConvertAcc YUV_HD CMY where
    convertColorAcc = helperColorConverter toCMY
