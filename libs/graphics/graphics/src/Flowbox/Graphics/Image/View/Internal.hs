---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.View.Internal (
    View (..),
    Name,
    ChanTree,
    Select (..),
    create',
    clean',
    modify'
) where

import           Data.Set
import           Data.List.Split
import qualified Data.Map as Map

import           Flowbox.Data.Channel           (ChannelTree(..))
import qualified Flowbox.Data.Channel           as ChanTree
import           Flowbox.Graphics.Image.Error   (Error(..))
import qualified Flowbox.Graphics.Image.Error   as Image
import           Flowbox.Graphics.Image.Channel (Channel(..))
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude                as P



type Name = String
type ChanTree = ChannelTree Channel.Name Channel

data Select = All
            | Default
            | Group { names :: Set Name }
            deriving (Show)

class View v where
    create   :: Name -> ChanTree -> Image.Result v
    empty    :: Name -> Image.Result v
    empty = flip create ChanTree.empty
    clean    :: v -> v
    modify   :: v -> ChanTree -> Image.Result v
    name     :: v -> Name -- Lens' v Name
    channels :: v -> ChanTree -- Lens' v (ChannelTree Channel.Name Channel)
    map      :: (Channel -> Channel) -> v -> Image.Result v
    map f v = modify v $ fmap f (channels v)
    --viewBounds :: Bounds
    --pixelAspectRatio :: AspectRatio

-- TODO: ukryć konstruktory

get :: View v => v -> Channel.Name -> Image.Result (Maybe Channel)
get v descriptor = case result of
    Left _    -> Left $ ChannelLookupError descriptor
    Right val -> Right val
    where result  = P.foldr f z nodes >>= ChanTree.get
          f p acc = acc >>= ChanTree.lookup p
          z       = ChanTree.zipper $ channels v
          nodes   = splitOn "." descriptor

append :: View view => Channel.Name -> Maybe Channel -> view -> Image.Result view
append descriptor val v = case val of
    Nothing   -> append'
    Just chan -> if last nodes == Channel.name chan
        then append'
        else Left $ ChannelNameError descriptor $ Channel.name chan
    where append' = case result of
              Left _   -> Left $ ChannelLookupError descriptor
              Right v' -> modify v $ ChanTree.tree v' -- v & channels .~ ChanTree.tree v'
          result   = P.foldl go z (init nodes) >>= ChanTree.append (last nodes) val >>= ChanTree.top
          go acc p = acc >>= ChanTree.lookup p
          z        = ChanTree.zipper $ channels v
          nodes    = splitOn "." descriptor

--map :: View view => f -> view -> Image.Result view
--map f v =

check :: ChanTree -> Bool
check EmptyNode = True
check (ChannelTree _ treeNodes) = Map.foldrWithKey check' True treeNodes
    where check' _ EmptyNode acc = acc
          check' key t@(ChannelTree chan _) acc = acc && comp key chan && check t
          comp key chan = case chan of
              Nothing    -> True
              Just chan' -> key == Channel.name chan'

validate :: ChanTree -> Image.Result ChanTree
validate t = if check t
             then return t
             else Left InvalidMap

create' :: View view => (Name -> ChanTree -> view) -> Name -> ChanTree -> Image.Result view
create' constructor viewName tree = validate tree >>= return . constructor viewName

clean' :: View view => (Name -> ChanTree -> view) -> view -> view
clean' constructor v = constructor (name v) ChanTree.empty

modify' :: View view => (Name -> ChanTree -> view) -> view -> ChanTree -> Image.Result view
modify' constructor v tree = validate tree >>= return . constructor (name v)

--mapWithWhitelist :: View view => ()

--set :: View view => view -> ChanTree -> Image.Result view
--set v t =
