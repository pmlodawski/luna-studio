---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.GraphView.Defaults (
    DefaultsMap,

    defaultsMapKey,
    getDefaults,
    setDefaults,
) where

import           Data.Map                                  (Map)
import qualified Data.Map                                as Map

import qualified Flowbox.Batch.Batch                     as Batch
import           Flowbox.Batch.GraphView.PortDescriptor    (PortDescriptor)
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import           Flowbox.Luna.Network.Graph.DefaultValue   (DefaultValue)
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import           Flowbox.Luna.Network.Graph.Node           (Node)



type DefaultsMap = Map PortDescriptor DefaultValue


defaultsMapKey :: String
defaultsMapKey = "Defaults-map"


getDefaults :: Node -> DefaultsMap
getDefaults node = defaults where
    attrs = Node.attributes node
    defaults = case Attributes.lookup Batch.attributeKey attrs of 
        Nothing         -> Attributes.empty
        Just batchAttrs -> case Map.lookup defaultsMapKey batchAttrs of
            Nothing -> Attributes.empty
            Just d  -> read d


setDefaults :: Node -> DefaultsMap -> Node
setDefaults node defaults = newNode where 
    attrs = Node.attributes node
    defaultsMapString = show defaults
    newAttrs = case Attributes.lookup Batch.attributeKey attrs of 
        Nothing         -> Attributes.insert Batch.attributeKey (Map.fromList [(defaultsMapKey, defaultsMapString)])          attrs
        Just batchAttrs -> Attributes.insert Batch.attributeKey (Map.insert     defaultsMapKey  defaultsMapString batchAttrs) attrs
    newNode = node { Node.attributes = newAttrs }
