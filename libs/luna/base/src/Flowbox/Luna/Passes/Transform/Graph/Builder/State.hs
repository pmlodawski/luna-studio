---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.Graph.Builder.State where

import           Control.Monad.State
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map

import qualified Flowbox.Luna.Data.AST.Common                   as AST
import qualified Flowbox.Luna.Data.Attributes                   as Attributes
import           Flowbox.Luna.Data.Graph.Edge                   (Edge (Edge))
import           Flowbox.Luna.Data.Graph.Graph                  (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                  as Graph
import           Flowbox.Luna.Data.Graph.Node                   (Node)
import qualified Flowbox.Luna.Data.Graph.Node                   as Node
import           Flowbox.Luna.Data.Graph.Port                   (InPort, OutPort)
import           Flowbox.Luna.Data.Pass.AliasInfo               (AliasInfo)
import qualified Flowbox.Luna.Data.Pass.AliasInfo               as AliasInfo
import           Flowbox.Luna.Data.PropertyMap                  (PropertyMap)
import qualified Flowbox.Luna.Data.PropertyMap                  as PropertyMap
import qualified Flowbox.Luna.Passes.Transform.Graph.Attributes as Attributes
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Builder.State"


type NodeMap = Map AST.ID (Node.ID, OutPort)


data GBState = GBState { _graph       :: Graph
                       , _nodeMap     :: NodeMap
                       , _aa          :: AliasInfo
                       , _propertyMap :: PropertyMap
                       } deriving (Show)

makeLenses(''GBState)


type GBStateM m = MonadState GBState m


make :: AliasInfo -> PropertyMap -> GBState
make = GBState Graph.empty Map.empty


addToNodeMap :: GBStateM m => AST.ID -> (Node.ID, OutPort) -> m ()
addToNodeMap k v = do nm <- getNodeMap
                      setNodeMap $ Map.insert k v nm


insNode :: GBStateM m => (Node.ID, Node) -> m ()
insNode node = do
    g <- getGraph
    setGraph $ Graph.insNode node g


insNodeWithFlags :: GBStateM m => (Node.ID, Node) -> Bool -> Bool -> m ()
insNodeWithFlags n@(nodeID, _) isFolded noAssignment = do
    insNode n
    when isFolded     $ setProperty Attributes.astFolded
    when noAssignment $ setProperty Attributes.astNoAssignment
    where setProperty key = getPropertyMap >>= setPropertyMap . PropertyMap.set nodeID Attributes.luna key Attributes.true


addNode :: GBStateM m => AST.ID -> OutPort -> Node -> Bool -> Bool -> m ()
addNode astID outPort node isFolded noAssignment = do
    insNodeWithFlags (astID, node) isFolded noAssignment
    addToNodeMap astID (astID, outPort)


connectNodes :: GBStateM m => Node.ID -> Node.ID -> Edge -> m ()
connectNodes srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge


connect :: GBStateM m => AST.ID -> Node.ID -> InPort -> m ()
connect srcID dstNID dstPort = do
    src <- gvmNodeMapLookUp srcID
    case src of
        Just (srcNID, srcPort) -> connectNodes srcNID dstNID $ Edge srcPort dstPort
        Nothing                -> return ()


getGraph :: GBStateM m => m Graph
getGraph = gets (view graph)


setGraph :: GBStateM m => Graph -> m ()
setGraph gr = modify (set graph gr)


getNodeMap :: GBStateM m => m NodeMap
getNodeMap = gets (view nodeMap)


setNodeMap :: GBStateM m => NodeMap -> m ()
setNodeMap nm = modify (set nodeMap nm)


getAAMap :: GBStateM m => m AliasInfo
getAAMap = gets (view aa)


setAAMap :: GBStateM m => AliasInfo -> m ()
setAAMap aa' = modify (set aa aa')


getPropertyMap :: GBStateM m => m PropertyMap
getPropertyMap = gets (view propertyMap)


setPropertyMap :: GBStateM m => PropertyMap -> m ()
setPropertyMap pm = modify (set propertyMap pm)


aaLookUp :: GBStateM m => AST.ID -> m AST.ID
aaLookUp astID = do aa' <- getAAMap
                    case IntMap.lookup astID $ aa' ^. AliasInfo.aliasMap of
                        Just a -> return a
                        _      -> return astID


nodeMapLookUp :: GBStateM m => AST.ID -> m (Maybe (Node.ID, OutPort))
nodeMapLookUp astID = do nm <- getNodeMap
                         return $ Map.lookup astID nm


gvmNodeMapLookUp :: GBStateM m => AST.ID -> m (Maybe (Node.ID, OutPort))
gvmNodeMapLookUp astID = aaLookUp astID >>= nodeMapLookUp
