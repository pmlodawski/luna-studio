---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Flowbox.Graphics.Image.Image (
    Image(..),
    insert,
    delete,
    lookup,
    update,
    map,
    singleton
) where

import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Traversable            (sequence)
import qualified Flowbox.Graphics.Image.View as View
import           Flowbox.Graphics.Image.Error
import           Flowbox.Prelude             hiding (views, lookup, sequence, map)

data Image view = Image { _views       :: Map.Map View.Name view
                        , _defaultView :: View.Select
                        }
makeLenses ''Image

image :: View.View v => Map.Map View.Name v -> View.Select -> Either Error (Image v)
image imgviews defaultview = if defaultview `Set.isSubsetOf` Map.keysSet imgviews
                                then newimg
                                else Left InvalidMap
    where keysMatchingNames = Map.foldrWithKey (\k v acc -> acc && View.name v == k) True imgviews
          newimg = if keysMatchingNames then return $ Image imgviews defaultview
                                        else Left InvalidMap

singleton :: View.View v => v -> Image v
singleton view = Image (Map.singleton name view) (Set.singleton name)
    where name = View.name view

insert :: View.View v => View.Name -> v -> Image v -> Either Error (Image v)
insert key value img = if View.name value == key
                             then return $ over views (Map.insert key value) img
                             else Left InvalidMap

delete :: View.View v => View.Name -> Image v -> Image v
delete key img = Image (Map.delete key $ img ^. views)
                       (Set.delete key $ img ^. defaultView)

lookup :: View.View v => View.Name -> Image v -> Maybe v
lookup key img = Map.lookup key (img ^. views)

update :: View.View v => (v -> Maybe v) -> View.Name -> Image v -> Either Error (Image v)
update f key img = case lookup key img >>= f of
    Just newval -> insert key newval img
    Nothing     -> return $ delete key img

map :: View.View v => (v -> v) -> Image v -> Image v --Either Error (Image v)
map lambda (Image vs dv) = Image (Map.map lambda $ vs) dv
--map lambda img = image (Map.map lambda $ img ^.views) (img ^. defaultView)
