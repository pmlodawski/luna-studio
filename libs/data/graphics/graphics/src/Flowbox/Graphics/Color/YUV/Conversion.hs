---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.YUV.Conversion where

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



toYUV :: (Elt a, IsFloating a, ColorConvert c YUV) => c (Exp a) -> YUV (Exp a)
toYUV = convertColor

instance ColorConvert YUV YUV where
    convertColor = id

instance ColorConvert RGB YUV where
    convertColor (RGB r' g' b') = YUV y' u' v'
        where y' = 0.299      * r' + 0.587   * g' + 0.114   * b'
              u' = (-0.14713) * r' - 0.28886 * g' + 0.436   * b'
              v' = 0.615      * r' - 0.51499 * g' - 0.10001 * b'

instance ColorConvert RGBA YUV where
    convertColor = helperColorConverter toYUV

instance ColorConvert HSV YUV where
    convertColor = helperColorConverter toYUV

instance ColorConvert HSL YUV where
    convertColor = helperColorConverter toYUV

instance ColorConvert CMY YUV where
    convertColor = helperColorConverter toYUV

instance ColorConvert CMYK YUV where
    convertColor = helperColorConverter toYUV

instance ColorConvert YUV_HD YUV where
    convertColor = helperColorConverter toYUV
