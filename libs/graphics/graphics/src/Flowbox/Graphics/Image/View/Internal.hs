---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.View.Internal where

import           Data.Set
import           Data.List.Split
import qualified Data.Map as Map

import           Flowbox.Data.Channel
import qualified Flowbox.Graphics.Image.Error   as Image
import           Flowbox.Graphics.Image.Channel (Channel)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude



type Name = String

data Select = All
            | Default
            | Group { names :: Set Name }
            deriving (Show)

class View v where
    name     :: Lens' v Name
    channels :: Lens' v (ChannelTree Channel.Name Channel)
    --viewBounds :: Bounds
    --pixelAspectRatio :: AspectRatio


--get :: View v => v -> String -> Image.Result (Maybe Channel)
--get v descriptor = rec parts (v ^. channels)
--    where parts = splitOn "." descriptor
--          rec [] _ = Left $ Image.ChannelLookupError descriptor
--          rec (x:[]) (ChannelTree children _) = case Map.lookup x children of
--              Nothing                -> Left $ Image.ChannelLookupError descriptor
--              Just (ChannelTree _ n) -> Right n
--          rec (x:xs) (ChannelTree children _) = case Map.lookup x children of
--              Nothing   -> Left $ Image.ChannelLookupError descriptor
--              Just node -> rec xs node

--insert :: View v => v -> String -> Channel -> Image.Result v
--insert v descriptor val = rec parts ()
