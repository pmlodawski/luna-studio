---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.YUV.Conversion where

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



toYUV :: (Elt a, IsFloating a, ColorConvertAcc c YUV) => c (Exp a) -> YUV (Exp a)
toYUV = convertColorAcc

instance ColorConvertAcc YUV YUV where
    convertColorAcc = id

instance ColorConvertAcc RGB YUV where
    convertColorAcc (RGB r' g' b') = YUV y' u' v'
        where y' = 0.299      * r' + 0.587   * g' + 0.114   * b'
              u' = (-0.14713) * r' - 0.28886 * g' + 0.436   * b'
              v' = 0.615      * r' - 0.51499 * g' - 0.10001 * b'

instance ColorConvertAcc RGBA YUV where
    convertColorAcc = helperColorConverter toYUV

instance ColorConvertAcc HSV YUV where
    convertColorAcc = helperColorConverter toYUV

instance ColorConvertAcc HSL YUV where
    convertColorAcc = helperColorConverter toYUV

instance ColorConvertAcc CMY YUV where
    convertColorAcc = helperColorConverter toYUV

instance ColorConvertAcc CMYK YUV where
    convertColorAcc = helperColorConverter toYUV

instance ColorConvertAcc YUV_HD YUV where
    convertColorAcc = helperColorConverter toYUV
