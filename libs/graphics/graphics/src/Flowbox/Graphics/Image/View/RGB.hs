---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.View.RGB where

import           Flowbox.Data.Channel
import           Flowbox.Graphics.Image.Channel (Channel)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.View.Internal (View)
import qualified Flowbox.Graphics.Image.View.Internal as View
import           Flowbox.Prelude



data RGB = RGB { _name'     :: View.Name
               , _channels' :: ChannelTree Channel.Name Channel
               }
         deriving (Show)
makeLenses ''RGB

instance View RGB where
    name     = name'
    channels = channels'
