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

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Flowbox.Batch.Batch                as Batch
import           Flowbox.Batch.GraphView.Value      (Value)
import qualified Flowbox.Luna.Data.Attributes       as Attributes
import           Flowbox.Luna.Data.Graph.Node       (Node)
import qualified Flowbox.Luna.Data.Graph.Node       as Node
import           Flowbox.Luna.Data.Graph.Port       (InPort)
import qualified Flowbox.Luna.Data.Graph.Properties as Proprties
import           Flowbox.Prelude



type DefaultsMap = Map InPort Value


defaultsMapKey :: String
defaultsMapKey = "Defaults-map"


getDefaults :: Node -> DefaultsMap
getDefaults node = case getKey $ node ^. (Node.properties . Proprties.attrs) of
    Nothing -> Map.empty
    Just d  -> read d
    where getKey = Attributes.get Batch.attributeKey defaultsMapKey


setDefaults :: Node -> DefaultsMap -> Node
setDefaults node defaults = 
    node & (Node.properties . Proprties.attrs)
        %~ Attributes.set Batch.attributeKey defaultsMapKey (show defaults)


