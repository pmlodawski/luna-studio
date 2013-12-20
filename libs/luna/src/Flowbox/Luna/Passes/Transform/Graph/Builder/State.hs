---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.Graph.Builder.State where

import           Control.Monad.State
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AliasAnalysis             (AA)
import qualified Flowbox.Luna.Data.AliasAnalysis             as AA
import qualified Flowbox.Luna.Data.AST.Utils                 as AST
import qualified Flowbox.Luna.Data.Graph.Default.DefaultsMap as DefaultsMap
import           Flowbox.Luna.Data.Graph.Default.Value       (Value)
import           Flowbox.Luna.Data.Graph.Edge                (Edge (Edge))
import           Flowbox.Luna.Data.Graph.Graph               (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph               as Graph
import           Flowbox.Luna.Data.Graph.Node                (Node)
import qualified Flowbox.Luna.Data.Graph.Node                as Node
import           Flowbox.Luna.Data.Graph.Port                (InPort, OutPort)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Builder.State"


type NodeMap = Map AST.ID (Node.ID, OutPort)


data GBState = GBState { graph   :: Graph
                       , nodeMap :: NodeMap
                       , aaMap   :: AA
                       } deriving (Show)


type GBStateM m = MonadState GBState m


make :: AA -> GBState
make = GBState Graph.make Map.empty


addToNodeMap :: GBStateM m => AST.ID -> (Node.ID, OutPort) -> m ()
addToNodeMap k v = do nm <- getNodeMap
                      setNodeMap $ Map.insert k v nm


--insNewNode :: GBStateM m => Node -> m Node.ID
--insNewNode node = do gr <- getGraph
--                     let (gr', nodeID) = Graph.insNewNode node gr
--                     setGraph gr'
--                     return nodeID

insNode :: GBStateM m => (Node.ID, Node) -> m ()
insNode n = do getGraph >>= setGraph . Graph.insNode n


addNode :: GBStateM m => AST.ID -> OutPort -> Node -> m ()
addNode astID outPort node = do
    insNode (astID, node)
    addToNodeMap astID (astID, outPort)


connect :: GBStateM m => Node.ID -> Node.ID -> Edge -> m ()
connect srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge


connectAST :: GBStateM m => AST.ID -> Node.ID -> InPort -> m ()
connectAST srcID dstNID dstPort = do
    (srcNID, srcPort) <- aaNodeMapLookUp srcID
    connect srcNID dstNID $ Edge srcPort dstPort


getGraph :: GBStateM m => m Graph
getGraph = get >>= return . graph


setGraph :: GBStateM m => Graph -> m ()
setGraph gr = do gm <- get
                 put gm { graph = gr }

getAAMap :: GBStateM m => m AA
getAAMap = get >>= return . aaMap


setAAMap :: GBStateM m => AA -> m ()
setAAMap aa = do gm <- get
                 put gm { aaMap = aa }


getNodeMap :: GBStateM m => m NodeMap
getNodeMap = get >>= return . nodeMap


setNodeMap :: GBStateM m => NodeMap -> m ()
setNodeMap nm = do gm <- get
                   put gm { nodeMap = nm }


aaLookUp :: GBStateM m => AST.ID -> m AST.ID
aaLookUp astID = do aa <- getAAMap
                    case IntMap.lookup astID $ AA.varmap aa of
                        Nothing -> return astID
                        Just a  -> return a


nodeMapLookUp :: GBStateM m => AST.ID -> m (Node.ID, OutPort)
nodeMapLookUp astID = getNodeMap >>= fromJust . (Map.lookup astID)


aaNodeMapLookUp :: GBStateM m => AST.ID -> m (Node.ID, OutPort)
aaNodeMapLookUp astID = aaLookUp astID >>= nodeMapLookUp


addNodeDefault :: GBStateM m => InPort -> Value -> Node.ID -> m ()
addNodeDefault dstPort value nodeID = do
    gr <- getGraph
    node <- Graph.lab gr nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
    let newNode = DefaultsMap.addDefault dstPort value node
    setGraph $ Graph.updateNode (nodeID, newNode) gr
