---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Flowbox.Graphics.Color.RGBA where

import Data.Typeable.Internal

--import           Flowbox.Graphics.Color.Internal
import           Flowbox.Prelude



data RGBA a = RGBA { rgbaR :: a, rgbaG :: a, rgbaB :: a, rgbaA :: a } deriving (Show,Typeable)
