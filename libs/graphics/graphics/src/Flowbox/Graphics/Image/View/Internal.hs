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
    get,
    append
) where

import           Data.Set        hiding (map)
import           Data.List.Split
import qualified Data.Map        as Map hiding (map)

import           Flowbox.Data.Channel           (ChannelTree(..))
import qualified Flowbox.Data.Channel           as ChanTree
import           Flowbox.Graphics.Image.Error   (Error(..))
import qualified Flowbox.Graphics.Image.Error   as Image
import           Flowbox.Graphics.Image.Channel (Channel(..))
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude                as P hiding (set, map)



type Name = String
type ChanTree = ChannelTree Channel.Name Channel

data Select = All
            | Default
            | Group { names :: Set Name }
            deriving (Show)

class View v where
    name     :: v -> Name -- Lens' v Name
    channels :: v -> ChanTree -- Lens' v (ChannelTree Channel.Name Channel)
    --viewBounds :: Bounds
    --pixelAspectRatio :: AspectRatio
    --
    empty    :: Name -> v
    set      :: ChanTree -> v -> v
    map      :: (Channel -> Channel) -> v -> v
    map f v = set (fmap f (channels v)) v

get :: View v => v -> Channel.Name -> Image.Result (Maybe Channel)
get v descriptor = case result of
    Left _    -> Left $ ChannelLookupError descriptor
    Right val -> Right val
    where result  = P.foldr f z nodes >>= ChanTree.get
          f p acc = acc >>= ChanTree.lookup p
          z       = ChanTree.zipper $ channels v
          nodes   = splitOn "." descriptor

append :: View view => Channel -> view -> view
append chan v = set (ChanTree.tree result') v
    where result = P.foldl go z (init nodes) >>= insert (last nodes) (Just chan) >>= ChanTree.top
          result' = case result of
              Right res -> res
              Left err  -> errorShitWentWrong $ "append (" ++ show err ++ ") "

          go acc p   = let res = acc >>= ChanTree.lookup p in case res of
              Right _ -> res
              Left  _ -> acc >>= ChanTree.append p Nothing >>= ChanTree.lookup p

          insert :: String -> Maybe Channel -> ChanTree.Zipper String Channel -> ChanTree.ZipperResult String Channel
          insert p v zipper = let res = ChanTree.lookup p zipper in case res of
              Right (ChannelTree _ oldmap, _) -> ChanTree.attach p (ChannelTree v oldmap) zipper
              Left _ -> ChanTree.append p v zipper

          z          = ChanTree.zipper $ channels v
          nodes      = splitOn "." descriptor
          descriptor = Channel.name chan

mapWithWhitelist :: View view => (Channel -> Channel) -> Channel.Select -> view -> view
mapWithWhitelist f whitelist = map lambda
    where lambda chan = if Channel.name chan `elem` whitelist
                            then f chan
                            else chan

-- == HELPERS == for error reporting

errorShitWentWrong :: String -> a
errorShitWentWrong fun =
  error (this_module ++ fun ++ ": cosmic radiation caused this function to utterly fail. Blame the monkeys and send us an error report.")

this_module :: String
this_module = "Flowbox.Graphics.Image.View."
