---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Image.Image (
    Image(..),
    insert,
    insertRequired,
    insertArbitrary,
    delete,
    lookup,
    --update,
    map,
    singleton,
    get
) where

import qualified Data.Set as Set
import           Flowbox.Data.Set (Set)

import           Flowbox.Graphics.Image.Channel (Channel(..))
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Error
import           Flowbox.Graphics.Image.View  (View(..), ViewData(..))
import qualified Flowbox.Graphics.Image.View  as View
import           Flowbox.Prelude              hiding (lookup, map, sequence, view, views)

data Image = Image { _views       :: Set View
                   } deriving (Show)

makeLenses ''Image

singleton :: View -> Image
singleton = Image . Set.singleton

insert :: View -> Image -> Image
insert view img = img & views %~ Set.insert view

insertRequired :: ViewData -> Image -> Image
insertRequired vd img = img & views %~ Set.insert (Required vd)

insertArbitrary :: ViewData -> Image -> Image
insertArbitrary vd img = img & views %~ Set.insert (Arbitrary vd)

delete :: View.Name -> Image -> Image
delete name img = img & views %~ Set.delete (dummyView name)

lookup :: View.Name -> Image -> Result View.View
lookup name img = case Set.lookupIndex (dummyView name) vs of
    Nothing  -> Left $ ViewLookupError name
    Just val -> Right $ Set.elemAt val vs
    where vs = img ^. views

-- TODO[KM]: lookupSelect - just like lookup, but taking View.Select as an argument (should it return a list of Eithers, or Either with a list inside - crashing if any of the names from Select does not exist)

-- TODO[KM]: figure out what to do about the `f` returning Maybe and lookup going through Either
--update :: (View.View -> Maybe View.View) -> View.Name -> Image -> Image
--update f name img = case lookup name img >>= f of
--    Just newval -> insert newval img
--    Nothing     -> delete name img

map :: (View.View -> View.View) -> Image -> Image
map lambda img = img & views %~ Set.map lambda

get :: Channel.Name -> View.Name -> Image -> Result (Maybe Channel)
get chanName viewName img = do
    view    <- lookup viewName img
    channel <- View.get view chanName
    return channel

--getChannel' :: Channel.Name -> Image -> Maybe Channel
--getChannel' chanName img = do
--    view    <- lookup (img ^. defaultView) img
--    channel <- View.get view chanName
--    return channel

--getChannels :: Channel.Select -> Image ->  Result ([Maybe Channel])

-- == HELPERS ==

dummyView :: View.Name -> View
dummyView name = Arbitrary $ View.empty name
