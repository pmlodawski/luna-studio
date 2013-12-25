module Flowbox.Graphics.Raster.Error where

import Flowbox.Prelude hiding (lookup, map)


data Error = ChannelLookupError { name :: String }

------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance Show Error where
    show err = case err of
        ChannelLookupError name -> "Channel lookup error: channel '" ++ name ++ "' not found"
