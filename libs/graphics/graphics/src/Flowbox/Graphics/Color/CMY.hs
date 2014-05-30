---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Flowbox.Graphics.Color.CMY where

import Data.Typeable.Internal

import           Flowbox.Graphics.Color.Internal
import           Flowbox.Prelude



data CMY a = CMY { cmyC :: a, cmyM :: a, cmyY :: a } deriving (Show,Typeable)

--type instance Color (CMY a) = CMY a
