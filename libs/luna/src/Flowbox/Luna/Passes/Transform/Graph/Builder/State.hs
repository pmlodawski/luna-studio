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
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Flowbox.Luna.Data.AliasAnalysis (AA)
import qualified Flowbox.Luna.Data.AST.Utils     as AST
import           Flowbox.Luna.Data.Graph.Edge    (Edge)
import           Flowbox.Luna.Data.Graph.Graph   (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph   as Graph
import           Flowbox.Luna.Data.Graph.Node    (Node)
import qualified Flowbox.Luna.Data.Graph.Node    as Node
import           Flowbox.Luna.Data.Graph.Port    (Port)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Builder.State"


type NodeMap = Map AST.ID (Node.ID, Port)


data GBState = GBState { graph   :: Graph
                       , nodemap :: NodeMap
                       , aa      :: AA
                       } deriving (Show)


type GBStateM m = MonadState GBState m


make :: AA -> GBState
make = GBState Graph.make Map.empty


addToMap :: GBStateM m => AST.ID -> (Node.ID, Port) -> m ()
addToMap k v = do gm <- get
                  put gm { nodemap = Map.insert k v $ nodemap gm }


connect :: GBStateM m => Node.ID -> Node.ID -> Edge -> m ()
connect srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge


insNewNode :: GBStateM m => Node -> m Node.ID
insNewNode node = do gr <- getGraph
                     let (gr', nodeID) = Graph.insNewNode node gr
                     setGraph gr'
                     return nodeID


getGraph :: GBStateM m => m Graph
getGraph = get >>= return . graph


setGraph :: GBStateM m => Graph -> m ()
setGraph gr = do gm <- get
                 put gm { graph = gr }
