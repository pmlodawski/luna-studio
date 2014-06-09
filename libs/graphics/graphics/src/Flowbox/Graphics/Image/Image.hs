---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Flowbox.Graphics.Image.Image ( 
    Image,
    insert,
    delete,
    lookup,
    update,
    map,
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
image imgviews defaultview = case defaultview of
    View.Group names -> if names `Set.isSubsetOf` Map.keysSet imgviews
                            then newimg
                            else Left InvalidMap
    _                 -> newimg
    where keysMatchingNames = Map.foldrWithKey (\k v acc -> acc && View.name v == k) True imgviews
          newimg = if keysMatchingNames then return $ Image imgviews defaultview
                                        else Left InvalidMap

insert :: View.View v => View.Name -> v -> Image v -> Either Error (Image v)
insert key value image = if View.name value == key
                             then return $ over views (Map.insert key value) image
                             else Left InvalidMap

delete :: View.View v => View.Name -> Image v -> Image v
delete key image = Image (Map.delete key $ image ^. views) $ case default_view of
    View.Group names -> View.Group $ Set.delete key names
    _                -> default_view
    where default_view = image ^. defaultView

lookup :: View.View v => View.Name -> Image v -> Maybe v
lookup key image = Map.lookup key (image ^. views)

update :: View.View v => (v -> Maybe v) -> View.Name -> Image v -> Either Error (Image v)
update f key image = case lookup key image >>= f of
    Just newval -> insert key newval image
    Nothing     -> return $ delete key image

map :: View.View v => (v -> v) -> Image v -> Either Error (Image v)
map lambda img = image (Map.map lambda $ img ^.views) (img ^. defaultView)
