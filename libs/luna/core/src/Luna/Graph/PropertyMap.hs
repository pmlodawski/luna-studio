---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

-- FIXME[pm]: Jezeli ten pas dostarcza danych ktore sa przesylane pomiedzy passami,
-- powinien znajdowac sie w Flowbox.Luna.Data.Pass
module Luna.Graph.PropertyMap (
    module Data.IntMap,
    module Luna.Graph.PropertyMap,
) where

import           Data.IntMap
import qualified Data.IntMap as IntMap
import qualified Data.Maybe  as Maybe

import           Flowbox.Prelude       hiding (set)
import qualified Luna.Graph.Attributes as Attributes
import qualified Luna.Graph.Node       as Node
import           Luna.Graph.Properties (Properties)
import qualified Luna.Graph.Properties as Properties



type PropertyMap = IntMap Properties


get :: Node.ID -> String -> String -> PropertyMap -> Maybe String
get nodeID spaceKey key propertyMap = do
    pm <- IntMap.lookup nodeID propertyMap
    let attrs = pm ^. Properties.attrs
    Attributes.get spaceKey key attrs


set :: Node.ID -> String -> String -> String -> PropertyMap -> PropertyMap
set nodeID spaceKey key value propertyMap = IntMap.insert nodeID newProperties propertyMap where
    oldProperties = Maybe.fromMaybe def (IntMap.lookup nodeID propertyMap)
    newProperties = oldProperties & Properties.attrs
        %~ Attributes.set spaceKey key value


move :: Node.ID -> Node.ID -> PropertyMap -> PropertyMap
move current new propertyMap = case IntMap.lookup current propertyMap of
    Nothing -> propertyMap
    Just k  -> IntMap.insert new k $ IntMap.delete current propertyMap
