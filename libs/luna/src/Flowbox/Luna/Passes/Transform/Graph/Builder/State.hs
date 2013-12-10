---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Luna.Passes.Transform.Graph.Builder.State where

import           Control.Monad.State
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Debug.Trace
import           Flowbox.Luna.Data.AliasAnalysis (AA)
import qualified Flowbox.Luna.Data.AliasAnalysis as AA
import qualified Flowbox.Luna.Data.AST.Utils     as AST
import           Flowbox.Luna.Data.Graph.Edge    (Edge)
import           Flowbox.Luna.Data.Graph.Graph   (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph   as Graph
import           Flowbox.Luna.Data.Graph.Node    (Node)
import qualified Flowbox.Luna.Data.Graph.Node    as Node
import           Flowbox.Luna.Data.Graph.Port    (OutPort)
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


addToMap :: GBStateM m => AST.ID -> (Node.ID, OutPort) -> m ()
addToMap k v = do gm <- get
                  put gm { nodeMap = Map.insert k v $ nodeMap gm }


insNewNode :: GBStateM m => Node -> m Node.ID
insNewNode node = do gr <- getGraph
                     let (gr', nodeID) = Graph.insNewNode node gr
                     setGraph gr'
                     return nodeID


addNode :: GBStateM m => AST.ID -> OutPort -> Node -> m Node.ID
addNode astID outPort node = do
    nodeID <- insNewNode node
    addToMap astID (nodeID, outPort)
    return nodeID

connect :: GBStateM m => Node.ID -> Node.ID -> Edge -> m ()
connect srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge


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
aaNodeMapLookUp astID = do s <- get
                           i <- traceShow ("Looking for " ++ (show astID) ++ "\n" ++ (show s)) $ aaLookUp astID
                           nodeMapLookUp i
