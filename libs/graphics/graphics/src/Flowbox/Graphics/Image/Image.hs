---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Flowbox.Graphics.Image.Image ( insert
                                    , delete
                                    , lookup
                                    , update
                                    ) where

import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Flowbox.Graphics.Image.View as View
import           Flowbox.Prelude             hiding (views, lookup)

data Image view = Image { _views       :: Map.Map View.Name view
                        , _defaultView :: View.Select
                        }
makeLenses ''Image

image :: View.View v => Map.Map View.Name v -> View.Select -> Maybe (Image v)
image imgviews defaultview = case defaultview of
    View.Group names -> if names `Set.isSubsetOf` Map.keysSet imgviews
                            then newimg
                            else Nothing
    _                 -> newimg
    where keysMatchingNames = Map.foldrWithKey (\k v acc -> acc && v ^. View.name == k) True imgviews
          newimg = if keysMatchingNames then Just $ Image imgviews defaultview
                                        else Nothing

insert :: View.View v => View.Name -> v -> Image v -> Maybe (Image v)
insert key value image = if value ^. View.name == key
                             then Just $ over views (Map.insert key value) image
                             else Nothing

delete :: View.View v => View.Name -> Image v -> Image v
delete key image = Image (Map.delete key $ image ^. views) $ case default_view of
    View.Group names -> View.Group $ Set.delete key names
    _                -> default_view
    where default_view = image ^. defaultView

lookup :: View.View v => View.Name -> Image v -> Maybe v
lookup key image = Map.lookup key (image ^. views)

update :: View.View v => (v -> Maybe v) -> View.Name -> Image v -> Maybe (Image v)
update f key image = case lookup key image >>= f of
    Just newval -> insert key newval image
    Nothing     -> Just $ delete key image

