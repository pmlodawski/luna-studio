---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Flowbox.Graphics.Color.Profile where

import Flowbox.Graphics.Color.CIE.XYZ
import Flowbox.Graphics.Color.Gamma
import Flowbox.Graphics.Color.Illuminants (Chromaticity(..))
import Flowbox.Prelude



class RGBProfile a b where
    type ReferenceWhite a :: *
    toXYZ      :: a 'Linear b -> XYZ b
    fromXYZ    :: XYZ b -> a 'Linear b
    whitepoint :: a c b -> ReferenceWhite a
    primaries  :: a c b -> (Chromaticity b, Chromaticity b, Chromaticity b)
