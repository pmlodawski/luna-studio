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
    singletonFromChans,
    views,
    insert,
    insertDefault,
    delete,
    --update,
    lookup,
    lookupPrimary,
    lookupDefault,
    map,
    get,
    getFromDefault,
    getChannels,
    getChannelsFromDefault
) where

import           Data.Maybe
import qualified Data.Set as Set
import           Flowbox.Data.Set (Set)

import           Flowbox.Graphics.Image.Channel (Channel(..))
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Error
import           Flowbox.Graphics.Image.View  (View(..))
import qualified Flowbox.Graphics.Image.View  as View
import           Flowbox.Prelude              hiding (empty, lookup, map, view, views)



data Image = Image { _arbitraryViews :: Set View
                   , _primaryView    :: Maybe View
                   } deriving (Show)

pattern DefaultView view <- Image _ view

makeLenses ''Image

empty :: Image
empty = Image Set.empty Nothing

singleton :: View -> Image
singleton = Image Set.empty . Just

singletonFromChans :: [Channel] -> Image
singletonFromChans chans = singleton $ foldr View.append View.emptyDefault chans

views :: Image -> Set View
views img = maybe views' views'' (img ^. primaryView)
    where views'    = img ^. arbitraryViews
          views'' v = Set.insert v views'

insert :: View -> Image -> Image
insert view img = img & arbitraryViews %~ Set.insert view

insertDefault :: View -> Image -> Image
insertDefault view img = maybe (img & primaryView .~ Just view) newDefault (img ^. primaryView)
    where newDefault oldView = insert oldView img & primaryView .~ Just view

delete :: View.Name -> Image -> Image
delete name img = maybe (removeView name) handleDefault (img ^. primaryView)
    where removeView vn   = img & arbitraryViews %~ Set.delete (dummyView vn)
          handleDefault v = if v ^. View.name == name
                                then img & primaryView .~ Nothing
                                else removeView name

lookup :: View.Name -> Image -> Result View
lookup name img = case Set.lookupIndex (dummyView name) vs of
    Nothing  -> Left $ ViewLookupError name
    Just val -> Right $ Set.elemAt val vs
    where vs = views img

lookupPrimary :: Image -> Result View
lookupPrimary img = fromMaybe (Left $ ViewLookupError primaryViewTag) $ fmap Right (img ^. primaryView)

lookupDefault :: Image -> Result View
lookupDefault = lookup View.defaultName

-- TODO[KM]: lookupSelect - just like lookup, but taking View.Select as an argument (should it return a list of Eithers, or Either with a list inside - crashing if any of the names from Select does not exist)

-- TODO[KM]: figure out what to do about the `f` returning Maybe and lookup going through Either
--update :: (View.View -> Maybe View.View) -> View.Name -> Image -> Image
--update f name img = case lookup name img >>= f of
--    Just newval -> insert newval img
--    Nothing     -> delete name img

map :: (View.View -> View.View) -> Image -> Image
map f img = img & arbitraryViews %~ Set.map f
                & primaryView %~ fmap f

get :: Channel.Name -> View.Name -> Image -> Result (Maybe Channel)
get chanName viewName img = do
    view    <- lookup viewName img
    channel <- View.get view chanName
    return channel

getFromDefault :: Channel.Name -> Image -> Result (Maybe Channel)
getFromDefault chanName img = case img ^. primaryView of
    Nothing -> Left $ ViewLookupError primaryViewTag
    Just v  -> View.get v chanName

getChannels :: Channel.Select -> View.Name -> Image ->  Result ([Maybe Channel])
getChannels chans viewName img = do
    view     <- lookup viewName img
    sequence $ fmap (View.get view) $ Set.toList chans

getChannelsFromDefault :: Channel.Select -> Image ->  Result ([Maybe Channel])
getChannelsFromDefault chans img = case img ^. primaryView of
    Nothing -> Left $ ViewLookupError primaryViewTag
    Just v  -> sequence $ fmap (View.get v) $ Set.toList chans


-- == HELPERS ==

dummyView :: View.Name -> View
dummyView name = View.empty name

primaryViewTag :: String
primaryViewTag = "- - primary view - -"
