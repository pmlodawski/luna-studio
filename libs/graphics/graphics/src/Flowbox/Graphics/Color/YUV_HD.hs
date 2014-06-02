---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Flowbox.Graphics.Color.YUV_HD where

import Data.Typeable.Internal

--import           Flowbox.Graphics.Color.Internal
import           Flowbox.Prelude



data YUV_HD a = YUV_HD { yuv_hdY :: a, yuv_hdU :: a, yuv_hdV :: a } deriving (Show,Typeable)
