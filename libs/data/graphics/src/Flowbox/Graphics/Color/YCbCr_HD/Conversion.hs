---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

module Flowbox.Graphics.Color.YCbCr_HD.Conversion where

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



toYCbCr_HD :: (Elt a, IsFloating a, ColorConvert c YCbCr_HD) => c (Exp a) -> YCbCr_HD (Exp a)
toYCbCr_HD = convertColor

instance ColorConvert YCbCr_HD YCbCr_HD where
    convertColor = id


-- Coefficients for this conversion from: http://discoverybiz.net/enu0/faq/faq_YUV_YCbCr_YPbPr.html

instance ColorConvert RGB YCbCr_HD where
    convertColor (RGB r' g' b') = YCbCr_HD y' cb cr
        where y' = kry * r' + kgy * g' + kby * b'
              cb = kru * r' + kgu * g' + kbu * b'
              cr = krv * r' + kgv * g' + kbv * b'
              kry = 0.2126
              kby = 0.0722
              kgy = 1 - kry - kby
              kru = -kry
              kgu = -kgy
              kbu = 1 - kby
              krv = 1 - kry
              kgv = -kgy
              kbv = -kby

instance ColorConvert RGBA YCbCr_HD where
    convertColor = helperColorConverter toYCbCr_HD

instance ColorConvert HSV YCbCr_HD where
    convertColor = helperColorConverter toYCbCr_HD

instance ColorConvert HSL YCbCr_HD where
    convertColor = helperColorConverter toYCbCr_HD

instance ColorConvert CMY YCbCr_HD where
    convertColor = helperColorConverter toYCbCr_HD

instance ColorConvert CMYK YCbCr_HD where
    convertColor = helperColorConverter toYCbCr_HD

instance ColorConvert YCbCr YCbCr_HD where
    convertColor = helperColorConverter toYCbCr_HD
