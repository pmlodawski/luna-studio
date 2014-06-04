---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Graphics.Color.Conversion where

import Data.Array.Accelerate



--type ColorAcc a = (Elt a, IsTuple a, Lift Exp a, Unlift Exp a) -- TODO: use this to put constraints on ColorConvertAcc parameters

--class (ColorAcc a, ColorAcc b) => ColorConvertAcc a b where
class ColorConvertAcc a b where
    convertColorAcc :: (Elt t, IsFloating t) => a (Exp t) -> b (Exp t)
