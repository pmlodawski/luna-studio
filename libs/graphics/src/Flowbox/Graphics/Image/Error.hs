---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.Error where

import Flowbox.Prelude hiding (lookup, map)


data Error = ChannelLookupError { name :: String }

------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance Show Error where
    show err = case err of
        ChannelLookupError name -> "Channel lookup error: channel '" ++ name ++ "' not found"

type Result a = Either Error a
