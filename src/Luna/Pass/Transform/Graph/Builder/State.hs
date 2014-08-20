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
import qualified Text.Read           as Read

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common              as AST
import           Luna.Data.AliasInfo          (AliasInfo)
import qualified Luna.Data.AliasInfo          as AliasInfo
import qualified Luna.Graph.Attributes        as Attributes
import qualified Luna.Graph.Attributes.Naming as Attributes
import           Luna.Graph.Edge              (Edge)
import qualified Luna.Graph.Edge              as Edge
import           Luna.Graph.Graph             (Graph)
import qualified Luna.Graph.Graph             as Graph
import           Luna.Graph.Node              (Node)
import qualified Luna.Graph.Node              as Node
import           Luna.Graph.Port              (InPort, OutPort)
import           Luna.Graph.PropertyMap       (PropertyMap)
import qualified Luna.Graph.PropertyMap       as PropertyMap
import           Luna.Info                    (apiVersion)
import           Luna.Pass.Pass               (Pass)



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Builder.State"


type NodeMap = Map AST.ID (Node.ID, OutPort)


data GBState = GBState { _graph        :: Graph
                       , _nodeMap      :: NodeMap
                       , _aa           :: AliasInfo
                       , _propertyMap  :: PropertyMap
                       , _prevoiusNode :: Node.ID
                       } deriving (Show)

makeLenses(''GBState)


type GBPass result = Pass GBState result


make :: AliasInfo -> PropertyMap -> Node.ID -> GBState
make = GBState Graph.empty Map.empty


addToNodeMap :: AST.ID -> (Node.ID, OutPort) -> GBPass ()
addToNodeMap k v = do nm <- getNodeMap
                      setNodeMap $ Map.insert k v nm


insNode :: (Node.ID, Node.Position -> Node) -> GBPass ()
insNode (nodeID, node) = do
    g   <- getGraph
    pos <- getPosition nodeID
    setGraph $ Graph.insNode (nodeID, node pos) g


insNodeWithFlags :: (Node.ID, Node.Position -> Node) -> Bool -> Bool -> GBPass ()
insNodeWithFlags n@(nodeID, _) isFolded assignment = do
    insNode n
    when isFolded   $ setFlag nodeID Attributes.astFolded
    when assignment $ setFlag nodeID Attributes.astAssignment


addNode :: AST.ID -> OutPort -> (Node.Position -> Node) -> Bool -> Bool -> GBPass ()
addNode astID outPort node isFolded assignment = do
    insNodeWithFlags (astID, node) isFolded assignment
    addToNodeMap astID (astID, outPort)


connectNodes :: Node.ID -> Node.ID -> Edge -> GBPass ()
connectNodes srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge


connect :: AST.ID -> Node.ID -> InPort -> GBPass ()
connect srcID dstNID dstPort = do
    found             <- gvmNodeMapLookUp srcID
    (srcNID, srcPort) <- found <??> "Graph.Builder.State.connect : cannot find " ++ show srcID
    connectNodes srcNID dstNID $ Edge.Data srcPort dstPort


connectMonadic :: Node.ID -> GBPass ()
connectMonadic nodeID = do
    prevID <- getPrevoiusNode
    setPrevoiusNode nodeID
    prevPos <- getPosition prevID
    currPos <- getPosition nodeID
    unless (prevPos < currPos) $
        setPosition nodeID (fst prevPos + 1, snd currPos)
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


nodeMapLookUp :: AST.ID -> GBPass (Maybe (Node.ID, OutPort))
nodeMapLookUp astID = do nm <- getNodeMap
                         return $ Map.lookup astID nm


gvmNodeMapLookUp :: AST.ID -> GBPass (Maybe (Node.ID, OutPort))
gvmNodeMapLookUp astID = do
    found <- nodeMapLookUp =<< Maybe.fromMaybe astID <$> aaLookUp astID
    if Maybe.isNothing found
        then nodeMapLookUp astID
        else return found


getPrevoiusNode :: GBPass Node.ID
getPrevoiusNode = gets (view prevoiusNode)


setPrevoiusNode :: Node.ID -> GBPass ()
setPrevoiusNode nodeID = modify (set prevoiusNode nodeID)


setFlag :: Node.ID -> String -> GBPass ()
setFlag nodeID key = setProperty nodeID key Attributes.true


setProperty :: Node.ID -> String -> String -> GBPass ()
setProperty nodeID key value =
    getPropertyMap >>=
    setPropertyMap . PropertyMap.set nodeID (show apiVersion) key value


getProperty :: Node.ID -> String -> GBPass (Maybe String)
getProperty nodeID key =
    PropertyMap.get nodeID (show apiVersion) key <$> getPropertyMap


getPosition :: Node.ID -> GBPass Node.Position
getPosition nodeID = do
    mprop <- getProperty nodeID Attributes.nodePosition
    case mprop of
        Nothing   -> return (0, 0)
        Just prop -> Read.readMaybe prop <??> "BuilderState.getPosition : cannot parse position for node " ++ show nodeID


setPosition :: Node.ID -> Node.Position -> GBPass ()
setPosition nodeID pos = do
    setProperty nodeID Attributes.nodePosition $ show pos
    graph' <- getGraph
    node   <- Graph.lab graph' nodeID <??> "BuilderState.setPosition : cannot find node with id = " ++ show nodeID
    setGraph $ Graph.updateNode (nodeID, node & Node.pos .~ pos) graph'
