---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Flowbox.Graphics.Image.Image (
    Image,
    pattern DefaultView,
    empty,
    singleton,
    views,
    insert,
    insertDefault,
    delete,
    --update,
    lookup,
    map,
    get,
    getFromDefault
) where

import qualified Data.Set as Set
import           Flowbox.Data.Set (Set)

import           Flowbox.Graphics.Image.Channel (Channel(..))
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Error
import           Flowbox.Graphics.Image.View  (View(..))
import qualified Flowbox.Graphics.Image.View  as View
import           Flowbox.Prelude              hiding (empty, lookup, map, sequence, view, views)



data Image = Image { _arbitraryViews :: Set View
                   , _defaultView    :: Maybe View
                   } deriving (Show)

pattern DefaultView view <- Image _ view

makeLenses ''Image

empty :: Image
empty = Image Set.empty Nothing

singleton :: View -> Image
singleton = Image Set.empty . Just

views :: Image -> Set View
views img = maybe views' views'' (img ^. defaultView)
    where views'    = img ^. arbitraryViews
          views'' v = Set.insert v views'

insert :: View -> Image -> Image
insert view img = img & arbitraryViews %~ Set.insert view

insertDefault :: View -> Image -> Image
insertDefault view img = maybe (img & defaultView .~ Just view) newDefault (img ^. defaultView)
    where newDefault oldView = insert oldView img & defaultView .~ Just view

delete :: View.Name -> Image -> Image
delete name img = maybe (removeView name) handleDefault (img ^. defaultView)
    where removeView vn   = img & arbitraryViews %~ Set.delete (dummyView vn)
          handleDefault v = if v ^. View.name == name
                                then img & defaultView .~ Nothing
                                else removeView name

lookup :: View.Name -> Image -> Result View.View
lookup name img = case Set.lookupIndex (dummyView name) vs of
    Nothing  -> Left $ ViewLookupError name
    Just val -> Right $ Set.elemAt val vs
    where vs = views img

-- TODO[KM]: lookupSelect - just like lookup, but taking View.Select as an argument (should it return a list of Eithers, or Either with a list inside - crashing if any of the names from Select does not exist)

-- TODO[KM]: figure out what to do about the `f` returning Maybe and lookup going through Either
--update :: (View.View -> Maybe View.View) -> View.Name -> Image -> Image
--update f name img = case lookup name img >>= f of
--    Just newval -> insert newval img
--    Nothing     -> delete name img

map :: (View.View -> View.View) -> Image -> Image
map f img = img & arbitraryViews %~ Set.map f
                & defaultView %~ fmap f

get :: Channel.Name -> View.Name -> Image -> Result (Maybe Channel)
get chanName viewName img = do
    view    <- lookup viewName img
    channel <- View.get view chanName
    return channel

getFromDefault :: Channel.Name -> Image -> Result (Maybe Channel)
getFromDefault chanName img = case img ^. defaultView of
    Nothing -> Left $ ViewLookupError "- - default view - -"
    Just v  -> View.get v chanName

--getChannels :: Channel.Select -> Image ->  Result ([Maybe Channel])

-- == HELPERS ==

dummyView :: View.Name -> View
dummyView name = View.empty name
