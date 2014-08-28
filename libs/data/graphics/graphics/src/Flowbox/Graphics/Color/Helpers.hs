---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.Helpers where

import           Data.Array.Accelerate

import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.RGB
import           Flowbox.Graphics.Color.RGB.Conversion
import           Flowbox.Prelude



helperColorConverter :: (Elt a, IsFloating a, ColorConvert c1 RGB, ColorConvert RGB c2) => (RGB (Exp a) -> c2 (Exp a)) -> c1 (Exp a) -> c2 (Exp a)
helperColorConverter converter color = converter . toRGB $ color
