---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Flowbox.Graphics.Image.Image (
    Image(..),
    insert,
    delete,
    lookup,
    update,
    map,
    singleton
) where

import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import           Data.Traversable             (sequence)
import           Flowbox.Graphics.Image.Error
import qualified Flowbox.Graphics.Image.View  as View
import           Flowbox.Prelude              hiding (lookup, map, sequence, views)

data Image = Image { _views       :: Map.Map View.Name View.View
                   , _defaultView :: View.Select
                   } deriving (Show)
makeLenses ''Image

image :: Map.Map View.Name View.View -> View.Select -> Either Error Image
image imgviews defaultview = if defaultview `Set.isSubsetOf` Map.keysSet imgviews
                                then newimg
                                else Left InvalidMap
    where keysMatchingNames = Map.foldrWithKey (\k v acc -> acc && View.name v == k) True imgviews
          newimg = if keysMatchingNames then return $ Image imgviews defaultview
                                        else Left InvalidMap

singleton :: View.View -> Image
singleton view = Image (Map.singleton name view) (Set.singleton name)
    where name = View.name view

insert :: View.View -> Image -> Image
insert view img = over views (Map.insert (View.name view) view) img

delete :: View.Name -> Image -> Image
delete key img = Image (Map.delete key $ img ^. views)
                       (Set.delete key $ img ^. defaultView)

lookup :: View.Name -> Image -> Maybe View.View
lookup key img = Map.lookup key (img ^. views)

update :: (View.View -> Maybe View.View) -> View.Name -> Image -> Image
update f key img = case lookup key img >>= f of
    Just newval -> insert newval img
    Nothing     -> delete key img

map :: (View.View -> View.View) -> Image -> Image --Either Error (Image v)
map lambda (Image vs dv) = Image (Map.map lambda vs) dv
--map lambda img = image (Map.map lambda $ img ^.views) (img ^. defaultView)
