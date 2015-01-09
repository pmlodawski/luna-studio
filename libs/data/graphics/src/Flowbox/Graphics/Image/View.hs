---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.View (
    View(..),
    ViewData(..),
    Name,
    MapTree,
    Select(..),
    only,
    get,
    append,
    name,
    channels,
    set,
    empty,
    map,
    remove,
    mapWithWhitelist
) where

import Data.List       as List (foldl')
import Data.List.Split
--import Data.Set        as Set hiding (empty, insert, map)
import Flowbox.Data.Set        as Set hiding (empty, insert, map)

import           Flowbox.Data.MapTree           (MapTree (..))
import qualified Flowbox.Data.MapTree           as MapTree
import           Flowbox.Graphics.Image.Channel (Channel (..))
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Error   (Error (..))
import qualified Flowbox.Graphics.Image.Error   as Image
import           Flowbox.Prelude                as P hiding (empty, map, set, view, only)



type Name        = String
type ChannelTree = MapTree Channel.Name Channel
data Select      = Selected (Set Name)
                 | All
                 deriving (Show)

data ViewData = ViewData { _name     :: Name
                         , _channels :: ChannelTree
                         }
                         deriving (Show)

data View = Required  ViewData
          | Arbitrary ViewData
          deriving (Show)

makeLenses ''ViewData

only :: Name -> Select
only = Selected . Set.singleton

fmap' :: (ViewData -> ViewData) -> View -> View
fmap' f (Required v)  = Required $ f v
fmap' f (Arbitrary v) = Arbitrary $ f v

apply :: (ViewData -> a) -> View -> a
apply f (Required v)  = f v
apply f (Arbitrary v) = f v

empty :: Name -> ViewData
empty name' = ViewData name' MapTree.empty

set :: ChannelTree -> View -> View
set t = fmap' setChans
    where setChans (ViewData name' _) = ViewData name' t

map :: (Channel -> Channel) -> View -> View
map f = fmap' mapChans
    where mapChans v = v & channels %~ fmap f

get :: View -> Channel.Name -> Image.Result (Maybe Channel)
get v descriptor = case result of
    Left _    -> Left $ ChannelLookupError descriptor
    Right val -> Right val
    where result = apply go v
          go v'  = gotoChannel descriptor v' >>= MapTree.get

append :: Channel -> View -> View
append chan v = set (MapTree.tree result') v
    where result  = List.foldl' go z (init nodes) >>= insert (last nodes) (Just chan) >>= MapTree.top
          result' = case result of
              Right res -> res
              Left err  -> errorShitWentWrong $ "append (" ++ show err ++ ") "

          go acc p   = let res = acc >>= MapTree.lookup p in case res of
              Right _ -> res
              Left  _ -> acc >>= MapTree.append p Nothing >>= MapTree.lookup p

          insert :: String -> Maybe Channel -> MapTree.Zipper String Channel -> MapTree.ZipperResult String Channel
          insert p v' zipper = let res = MapTree.lookup p zipper in case res of
              Right (MapTree _ oldmap, _) -> MapTree.attach p (MapTree v' oldmap) zipper
              Right (EmptyNode, _)        -> errorShitWentWrong $ "append.insert (found an EmptyNode oO) "
              Left _ -> MapTree.append p v' zipper

          z          = MapTree.zipper $ apply _channels v
          nodes      = splitOn "." descriptor
          descriptor = Channel.name chan

remove :: Name -> View -> Image.Result View
remove name' view = case apply (gotoChannel name') view of
    Left _    -> Left $ ChannelLookupError name'
    Right val -> case MapTree.delete val of
        Left _     -> Left $ ChannelLookupError "can it really happen?"
        Right tree -> pure $ set (MapTree.tree $ MapTree.top' tree) view

gotoChannel :: Name -> ViewData -> MapTree.ZipperResult Channel.Name Channel
gotoChannel name' view = result
    where result        = List.foldl' go startingPoint nodes
          go tree name'' = tree >>= MapTree.lookup name''
          startingPoint = MapTree.zipper $ _channels view
          nodes         = splitOn "." name'

mapWithWhitelist :: (Channel -> Channel) -> Channel.Select -> View -> View
mapWithWhitelist f whitelist = map lambda
    where lambda chan = if Channel.name chan `elem` whitelist
                            then f chan
                            else chan

-- == HELPERS == for error reporting

errorShitWentWrong :: String -> a
errorShitWentWrong fun =
  error (thisModule ++ fun ++ ": cosmic radiation caused this function to utterly fail. Blame the monkeys and send us an error report.")

thisModule :: String
thisModule = "Flowbox.Graphics.Image.View."

-- == INSTANCES ==

instance Eq ViewData where
    a == b = _name a == _name b

instance Ord ViewData where
  compare a b = _name a `compare` _name b

instance Eq View where
  a == b = apply _name a == apply _name b

instance Ord View where
  compare a b = apply _name a `compare` apply _name b
