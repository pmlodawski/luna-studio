---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.View (
    View(..),
    Name,
    MapTree,
    Select(..),
    defaultName,
    only,
    empty,
    emptyDefault,
    get,
    append,
    name,
    channels,
    set,
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

data View = View { _name     :: Name
                 , _channels :: ChannelTree
                 }
                 deriving (Show)

makeLenses ''View

defaultName :: Name
defaultName = "default"

only :: Name -> Select
only = Selected . Set.singleton

empty :: Name -> View
empty name' = View name' MapTree.empty

emptyDefault :: View
emptyDefault = empty defaultName

set :: ChannelTree -> View -> View
set t v = v & channels .~ t

map :: (Channel -> Channel) -> View -> View
map f v = v & channels %~ fmap f

get :: View -> Channel.Name -> Image.Result (Maybe Channel)
get v descriptor = case result of
    Left _    -> Left $ ChannelLookupError descriptor
    Right val -> Right val
    where result = go v
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

          z          = MapTree.zipper $ _channels v
          nodes      = splitOn "." descriptor
          descriptor = Channel.name chan

remove :: Name -> View -> Image.Result View
remove name' view = case (gotoChannel name') view of
    Left _    -> Left $ ChannelLookupError name'
    Right val -> case MapTree.delete val of
        Left _     -> Left $ ChannelLookupError "can it really happen?"
        Right tree -> pure $ set (MapTree.tree $ MapTree.top' tree) view

gotoChannel :: Name -> View -> MapTree.ZipperResult Channel.Name Channel
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

instance Eq View where
    a == b = _name a == _name b

instance Ord View where
  compare a b = _name a `compare` _name b
