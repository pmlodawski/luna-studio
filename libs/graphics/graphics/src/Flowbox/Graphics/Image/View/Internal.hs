---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.View.Internal where

import Data.Set

import           Flowbox.Data.Channel
import           Flowbox.Graphics.Image.Channel (Channel)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude



type Name = String

data Select = All
            | Default
            | Group { names :: Set Name }
            deriving (Show)

class View view where
    name     :: Lens' view Name
    channels :: Lens' view (ChannelTree Channel.Name Channel)
    --viewBounds :: Bounds
    --pixelAspectRatio :: AspectRatio
