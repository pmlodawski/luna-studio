---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Data.Graph.View.Default.DefaultsMap where

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Luna.Data.Graph.Attributes          as Attributes
import qualified Luna.Data.Graph.Node                as Node
import           Luna.Data.Graph.View.Default.Value  (Value)
import           Luna.Data.Graph.View.PortDescriptor (PortDescriptor)
import           Luna.Data.Graph.PropertyMap         (PropertyMap)
import qualified Luna.Data.Graph.PropertyMap         as PropertyMap
import           Flowbox.Prelude
import qualified Luna.Info                           as Info



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
