---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Graphics.Color.Conversion where

import Data.Array.Accelerate



class ColorConvert a b where
    convertColor :: (Elt t, IsFloating t) => a (Exp t) -> b (Exp t)
