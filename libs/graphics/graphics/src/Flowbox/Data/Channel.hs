---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Data.Channel where

import Data.Map
import Data.List.Split

import qualified Flowbox.Graphics.Image.Error as Image
import 			 Flowbox.Prelude



data ChannelTree name value = ChannelTree  { _children :: Map name (ChannelTree name value)
                                           , _channel  :: Maybe value
                                           }
makeLenses ''ChannelTree
