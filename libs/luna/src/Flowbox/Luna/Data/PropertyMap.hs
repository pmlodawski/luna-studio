---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.PropertyMap (
    module Data.IntMap,
    module Flowbox.Luna.Data.PropertyMap,
) where

import           Data.IntMap
import qualified Data.IntMap as IntMap

import qualified Flowbox.Luna.Data.Attributes       as Attributes
import qualified Flowbox.Luna.Data.Graph.Node       as Node
import           Flowbox.Luna.Data.Graph.Properties (Properties)
import qualified Flowbox.Luna.Data.Graph.Properties as Properties
import           Flowbox.Prelude                    hiding (set)



type PropertyMap = IntMap Properties


get :: Node.ID -> String -> String -> PropertyMap -> Maybe String
get nodeID spaceKey key propertyMap = do
    pm <- IntMap.lookup nodeID propertyMap
    let attrs = pm ^. Properties.attrs
    Attributes.get spaceKey key attrs


set :: Node.ID -> String -> String -> String -> PropertyMap -> PropertyMap
set nodeID spaceKey key value propertyMap = IntMap.insert nodeID newProperties propertyMap where
    oldProperties  = case IntMap.lookup nodeID propertyMap of
                        Nothing         -> Properties.empty
                        Just properties -> properties
    newProperties = oldProperties & Properties.attrs
        %~ Attributes.set spaceKey key value


move :: Node.ID -> Node.ID -> PropertyMap -> PropertyMap
move current new propertyMap = case IntMap.lookup current propertyMap of
    Nothing -> propertyMap
    Just k  -> IntMap.insert new k $ IntMap.delete current propertyMap
