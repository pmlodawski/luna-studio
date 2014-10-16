---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.Pass.Transform.Graph.Builder.State where

import           Control.Monad.State
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common           as AST
import           Luna.Data.AliasInfo       (AliasInfo)
import qualified Luna.Data.AliasInfo       as AliasInfo
import           Luna.Graph.Edge           (Edge)
import qualified Luna.Graph.Edge           as Edge
import           Luna.Graph.Flags          (Flags)
import qualified Luna.Graph.Flags          as Flags
import           Luna.Graph.Graph          (Graph)
import qualified Luna.Graph.Graph          as Graph
import           Luna.Graph.Node           (Node)
import qualified Luna.Graph.Node           as Node
import           Luna.Graph.Node.Position  (Position)
import           Luna.Graph.Port           (Port)
import           Luna.Graph.PropertyMap    (PropertyMap)
import qualified Luna.Graph.PropertyMap    as PropertyMap
import           Luna.Pass.Pass            (Pass)



logger :: Logger
logger = getLogger $(moduleName)


type NodeMap = Map AST.ID (Node.ID, Port)


data GBState = GBState { _graph        :: Graph
                       , _nodeMap      :: NodeMap
                       , _aa           :: AliasInfo
                       , _propertyMap  :: PropertyMap
                       , _foldNodes    :: Bool
                       , _prevoiusNode :: Node.ID
                       } deriving (Show)

makeLenses(''GBState)


type GBPass result = Pass GBState result


make :: AliasInfo -> PropertyMap -> Bool -> Node.ID -> GBState
make = GBState Graph.empty Map.empty


addToNodeMap :: AST.ID -> (Node.ID, Port) -> GBPass ()
addToNodeMap k v = do nm <- getNodeMap
                      setNodeMap $ Map.insert k v nm


insNode :: (Node.ID, Position -> Node) -> GBPass ()
insNode (nodeID, node) = do
    g   <- getGraph
    pos <- Maybe.fromMaybe (0,0) <$> getPosition nodeID
    setGraph $ Graph.insNode (nodeID, node pos) g


insNodeWithFlags :: (Node.ID, Position -> Node) -> Bool -> Bool -> GBPass ()
insNodeWithFlags n@(nodeID, _) isFolded assignment = do
    insNode n
    when isFolded   $ modifyFlags (Flags.astFolded     .~ Just True) nodeID
    when assignment $ modifyFlags (Flags.astAssignment .~ Just True) nodeID


addNode :: AST.ID -> Port -> (Position -> Node) -> Bool -> Bool -> GBPass ()
addNode astID outPort node isFolded assignment = do
    insNodeWithFlags (astID, node) isFolded assignment
    addToNodeMap astID (astID, outPort)


connectNodes :: Node.ID -> Node.ID -> Edge -> GBPass ()
connectNodes srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge


connect :: AST.ID -> Node.ID -> Port -> GBPass ()
connect srcID dstNID dstPort = do
    found             <- gvmNodeMapLookUp srcID
    (srcNID, srcPort) <- found <??> "Graph.Builder.State.connect : cannot find " ++ show srcID
    connectNodes srcNID dstNID $ Edge.Data srcPort dstPort


connectMonadic :: Node.ID -> GBPass ()
connectMonadic nodeID = do
    prevID <- getPrevoiusNode
    setPrevoiusNode nodeID
    prevPos <- Maybe.fromMaybe (0,0) <$> getPosition prevID
    currPos <- getPosition nodeID
    when (Maybe.isNothing currPos) $
        setPosition nodeID (fst prevPos + 10, snd prevPos)
    connectNodes prevID nodeID Edge.Monadic


getGraph :: GBPass Graph
getGraph = gets (view graph)


setGraph :: Graph -> GBPass ()
setGraph gr = modify (set graph gr)


getNodeMap :: GBPass NodeMap
getNodeMap = gets (view nodeMap)


setNodeMap :: NodeMap -> GBPass ()
setNodeMap nm = modify (set nodeMap nm)


getAAMap :: GBPass AliasInfo
getAAMap = gets (view aa)


setAAMap :: AliasInfo -> GBPass ()
setAAMap aa' = modify (set aa aa')


getPropertyMap :: GBPass PropertyMap
getPropertyMap = gets (view propertyMap)


setPropertyMap :: PropertyMap -> GBPass ()
setPropertyMap pm = modify (set propertyMap pm)


aaLookUp :: AST.ID -> GBPass (Maybe AST.ID)
aaLookUp astID = do aa' <- getAAMap
                    return $ IntMap.lookup astID $ aa' ^. AliasInfo.aliasMap


nodeMapLookUp :: AST.ID -> GBPass (Maybe (Node.ID, Port))
nodeMapLookUp astID = do nm <- getNodeMap
                         return $ Map.lookup astID nm


gvmNodeMapLookUp :: AST.ID -> GBPass (Maybe (Node.ID, Port))
gvmNodeMapLookUp astID = do
    found <- nodeMapLookUp =<< Maybe.fromMaybe astID <$> aaLookUp astID
    if Maybe.isNothing found
        then nodeMapLookUp astID
        else return found


getPrevoiusNode :: GBPass Node.ID
getPrevoiusNode = gets (view prevoiusNode)


setPrevoiusNode :: Node.ID -> GBPass ()
setPrevoiusNode nodeID = modify (set prevoiusNode nodeID)


modifyFlags :: (Flags -> Flags) -> Node.ID -> GBPass ()
modifyFlags fun nodeID =
    getPropertyMap >>= setPropertyMap . PropertyMap.modifyFlags fun nodeID


getFlags :: Node.ID -> GBPass (Maybe Flags)
getFlags nodeID = PropertyMap.getFlags nodeID <$> getPropertyMap


getPosition :: Node.ID -> GBPass (Maybe Position)
getPosition nodeID =
    do flags <- getFlags nodeID
       return $ view Flags.nodePosition =<< flags


setPosition :: Node.ID -> Position -> GBPass ()
setPosition nodeID pos = do
    modifyFlags (Flags.nodePosition .~ Just pos) nodeID
    graph' <- getGraph
    node   <- Graph.lab graph' nodeID <??> "BuilderState.setPosition : cannot find node with id = " ++ show nodeID
    setGraph $ Graph.updateNode (nodeID, node & Node.pos .~ pos) graph'


getGraphFolded :: Node.ID -> GBPass Bool
getGraphFolded nodeID = do
    foldSetting <- gets (view foldNodes)
    if foldSetting
        then flip Flags.isSet' (view Flags.graphFolded) <$> getFlags nodeID
        else return False
