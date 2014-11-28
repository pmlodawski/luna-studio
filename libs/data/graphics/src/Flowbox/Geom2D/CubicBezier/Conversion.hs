---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Geom2D.CubicBezier.Conversion where

import qualified Geom2D.CubicBezier.Basic as Cubic

import Flowbox.Geom2D.Conversion
import Flowbox.Geom2D.CubicBezier
import Flowbox.Prelude



fcb2gcb :: CubicBezier Double -> Cubic.CubicBezier
fcb2gcb (CubicBezier (fp2gp -> p1) (fp2gp -> p2) (fp2gp -> p3) (fp2gp -> p4)) = Cubic.CubicBezier p1 p2 p3 p4

gcb2fcb :: Cubic.CubicBezier -> CubicBezier Double
gcb2fcb (Cubic.CubicBezier (gp2fp -> p1) (gp2fp -> p2) (gp2fp -> p3) (gp2fp -> p4)) = CubicBezier p1 p2 p3 p4
