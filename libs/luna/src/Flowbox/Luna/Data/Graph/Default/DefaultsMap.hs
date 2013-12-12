---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Graph.Default.DefaultsMap where

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Flowbox.Luna.Data.Attributes                       as Attributes
import           Flowbox.Luna.Data.Graph.Node                       (Node)
import qualified Flowbox.Luna.Data.Graph.Node                       as Node
import           Flowbox.Luna.Data.Graph.Port                       (InPort)
import qualified Flowbox.Luna.Data.Graph.Properties                 as Proprties
import           Flowbox.Luna.Data.Graph.Default.Value (Value)
import           Flowbox.Prelude



type DefaultsMap = Map InPort Value


defaultsMapKey :: String
defaultsMapKey = "Defaults-map"


getDefaults :: Node -> DefaultsMap
getDefaults node = case getKey $ node ^. (Node.properties . Proprties.attrs) of
    Nothing -> Map.empty
    Just d  -> read d
    where getKey = Attributes.get Attributes.lunaAttributeKey defaultsMapKey


setDefaults :: Node -> DefaultsMap -> Node
setDefaults node defaults =
    node & (Node.properties . Proprties.attrs)
        %~ Attributes.set Attributes.lunaAttributeKey defaultsMapKey (show defaults)


