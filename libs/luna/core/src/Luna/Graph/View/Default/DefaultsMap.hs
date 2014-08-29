---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Graph.View.Default.DefaultsMap where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Flowbox.Prelude
import qualified Luna.Graph.Attributes          as Attributes
import qualified Luna.Graph.Node                as Node
import           Luna.Graph.PropertyMap         (PropertyMap)
import qualified Luna.Graph.PropertyMap         as PropertyMap
import           Luna.Graph.View.Default.Value  (Value)
import           Luna.Graph.View.PortDescriptor (PortDescriptor)
import qualified Luna.Info                      as Info



type DefaultsMap = Map PortDescriptor (Node.ID, Value)


defaultsMapKey :: String
defaultsMapKey = "Defaults-map"


getDefaultsMap :: Node.ID -> PropertyMap -> DefaultsMap
getDefaultsMap nodeID propertyMap = case PropertyMap.get nodeID (show Info.apiVersion) defaultsMapKey propertyMap of
    Nothing -> mempty
    Just d  -> read d


setDefaultsMap :: DefaultsMap -> Node.ID -> PropertyMap -> PropertyMap
setDefaultsMap defaults nodeID =
    PropertyMap.set nodeID (show Info.apiVersion) defaultsMapKey (show defaults)


addDefault :: PortDescriptor -> (Node.ID, Value) -> Node.ID -> PropertyMap -> PropertyMap
addDefault dstPort default_ nodeID propertyMap = newPropertyMap where
     newDefaults    = Map.insert dstPort default_ $ getDefaultsMap nodeID propertyMap
     newPropertyMap = setDefaultsMap newDefaults nodeID propertyMap


removeDefault :: PortDescriptor -> Node.ID -> PropertyMap -> PropertyMap
removeDefault dstPort nodeID propertyMap = newPropertyMap where
    newDefaults    = Map.delete dstPort $ getDefaultsMap nodeID propertyMap
    newPropertyMap = setDefaultsMap newDefaults nodeID propertyMap
