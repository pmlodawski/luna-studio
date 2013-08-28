---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Defaults (
    nodeDefaults,
    setNodeDefault,
    removeNodeDefault,
) where

import           Data.Map                                  (Map)
import qualified Data.Map                                as Map

import qualified Flowbox.Batch.Batch                     as Batch
import           Flowbox.Batch.Batch                       (Batch(..))
import           Flowbox.Batch.Handlers.Common             (noresult, readonly, nodeOp)
import qualified Flowbox.Batch.Project.Project           as Project
import qualified Flowbox.Luna.Lib.Library                as Library
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Graph.DefaultValue   (DefaultValue)
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import           Flowbox.Luna.Network.Graph.Node           (Node)


defaultsMapKey :: String
defaultsMapKey = "Defaults-map"

type DefaultsMap = Map [Int] DefaultValue


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
        Nothing -> Attributes.fromList [(Batch.attributeKey, Map.fromList [(defaultsMapKey, defaultsMapString)])]
        Just batchAttrs -> Attributes.insert Batch.attributeKey (Map.insert defaultsMapKey defaultsMapString batchAttrs) attrs
    newNode = node { Node.attributes = newAttrs }


nodeDefaults :: Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String DefaultsMap
nodeDefaults nodeID defID libID projectID  = readonly . nodeOp nodeID defID libID projectID (\_ node -> let
    defaults = getDefaults node
    in Right (node, defaults))


setNodeDefault :: [Int] -> DefaultValue
               -> Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
setNodeDefault dstPort value nodeID defID libID projectID = 
    noresult . nodeOp nodeID defID libID projectID (\_ node -> let 
        newDefaults = Map.insert dstPort value
                    $ getDefaults node
        newNode = setDefaults node newDefaults
        in Right (newNode, ()))


removeNodeDefault :: [Int]
                  -> Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
removeNodeDefault dstPort nodeID defID libID projectID = 
    noresult . nodeOp nodeID defID libID projectID (\_ node -> let 
        newDefaults = Map.delete dstPort
                    $ getDefaults node
        newNode = setDefaults node newDefaults
        in Right (newNode, ()))