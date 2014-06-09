---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.View.Internal where

import           Data.Set
import           Data.List.Split
--import qualified Data.Map as Map

import           Flowbox.Data.Channel           as ChanTree
import           Flowbox.Graphics.Image.Error   (Error(..))
import qualified Flowbox.Graphics.Image.Error   as Image
import           Flowbox.Graphics.Image.Channel (Channel(..))
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude                as P



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


-- TODO: ukryć konstruktory

empty :: (View view, Ord name) => (name -> ChannelTree name value -> view) -> name -> view
empty v name = v name ChanTree.empty

get :: View v => v -> Channel.Name -> Image.Result (Maybe Channel)
get v descriptor = case result of
    Left _    -> Left $ ChannelLookupError descriptor
    Right val -> Right val
    where result  = P.foldr f z nodes >>= ChanTree.get
          f p acc = acc >>= ChanTree.lookup p
          z       = zipper $ v ^. channels
          nodes   = splitOn "." descriptor

append :: View view => Channel.Name -> Maybe Channel -> view -> Image.Result view
append descriptor val v = case val of
    Nothing   -> append'
    Just chan -> if last nodes == Channel.name chan
        then append'
        else Left $ ChannelNameError descriptor $ Channel.name chan
    where append' = case result of
              Left _   -> Left $ ChannelLookupError descriptor
              Right v' -> return $ v & channels .~ ChanTree.tree v'
          result   = P.foldl go z (init nodes) >>= ChanTree.append (last nodes) val >>= ChanTree.top
          go acc p = acc >>= ChanTree.lookup p
          z        = zipper $ v ^. channels
          nodes    = splitOn "." descriptor
