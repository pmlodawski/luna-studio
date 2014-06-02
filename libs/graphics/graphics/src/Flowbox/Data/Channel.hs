---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Data.Channel where

import Data.Map

import Flowbox.Prelude



data ChannelTree name value = ChannelTree  { children :: Map name (ChannelTree name value)
                                           , channel  :: Maybe value
                                           }
