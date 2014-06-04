---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.YUV_HD.Conversion where

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



toYUV_HD :: (Elt a, IsFloating a, ColorConvert c YUV_HD) => c (Exp a) -> YUV_HD (Exp a)
toYUV_HD = convertColor

instance ColorConvert YUV_HD YUV_HD where
    convertColor = id

instance ColorConvert RGB YUV_HD where
    convertColor (RGB r' g' b') = YUV_HD y' u' v'
        where y' = 0.2126     * r' + 0.7152  * g' + 0.0722  * b'
              u' = (-0.09991) * r' - 0.33609 * g' + 0.436   * b'
              v' = 0.615      * r' - 0.55861 * g' - 0.05639 * b'

instance ColorConvert RGBA YUV_HD where
    convertColor = helperColorConverter toYUV_HD

instance ColorConvert HSV YUV_HD where
    convertColor = helperColorConverter toYUV_HD

instance ColorConvert HSL YUV_HD where
    convertColor = helperColorConverter toYUV_HD

instance ColorConvert CMY YUV_HD where
    convertColor = helperColorConverter toYUV_HD

instance ColorConvert CMYK YUV_HD where
    convertColor = helperColorConverter toYUV_HD

instance ColorConvert YUV YUV_HD where
    convertColor = helperColorConverter toYUV_HD
