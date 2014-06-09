---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.Error where

import qualified Flowbox.Graphics.Image.Channel as Channel
import			 Flowbox.Prelude



data Error = ChannelLookupError { name :: Channel.Name }
		   | ChannelNameError { descriptor :: Channel.Name, name :: Channel.Name }

instance Show Error where
    show err = case err of
        ChannelLookupError chanName -> "Channel lookup error: channel '" ++ chanName ++ "' not found"
        ChannelNameError descriptorVal chanName -> "Channel naming error: last part of the descriptor '" ++ descriptorVal ++ "' does not match '" ++ chanName ++ "'"

type Result a = Either Error a
