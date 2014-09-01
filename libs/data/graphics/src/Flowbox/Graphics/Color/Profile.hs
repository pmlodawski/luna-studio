---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Flowbox.Graphics.Color.Profile where

import Flowbox.Graphics.Color.CIE.XYZ
import Flowbox.Prelude



class RGBProfile a b where
    type ReferenceWhite a :: *
    toXYZ   :: a b -> XYZ (ReferenceWhite a) b
    fromXYZ :: XYZ (ReferenceWhite a) b -> a b
