---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Graph.Default.DefaultsMap where

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Flowbox.Luna.Data.Attributes          as Attributes
import           Flowbox.Luna.Data.Graph.Default.Value (Value)
import           Flowbox.Luna.Data.Graph.Node          (Node)
import qualified Flowbox.Luna.Data.Graph.Node          as Node
import           Flowbox.Luna.Data.Graph.Port          (InPort)
import qualified Flowbox.Luna.Data.Graph.Properties    as Proprties
import           Flowbox.Prelude



type DefaultsMap = Map InPort Value


defaultsMapKey :: String
defaultsMapKey = "Defaults-map"


getDefaultsMap :: Node -> DefaultsMap
getDefaultsMap node = case getKey $ node ^. (Node.properties . Proprties.attrs) of
    Nothing -> Map.empty
    Just d  -> read d
    where getKey = Attributes.get Attributes.lunaAttributeKey defaultsMapKey


setDefaultsMap :: DefaultsMap -> Node -> Node
setDefaultsMap defaults node =
    node & (Node.properties . Proprties.attrs)
        %~ Attributes.set Attributes.lunaAttributeKey defaultsMapKey (show defaults)


addDefault :: InPort -> Value -> Node -> Node
addDefault dstPort value node = newNode where
     newDefaults = Map.insert dstPort value $ getDefaultsMap node
     newNode     = setDefaultsMap newDefaults node


removeDefault :: InPort -> Node -> Node
removeDefault dstPort node = newNode where
    newDefaults  = Map.delete dstPort $ getDefaultsMap node
    newNode      = setDefaultsMap newDefaults node
