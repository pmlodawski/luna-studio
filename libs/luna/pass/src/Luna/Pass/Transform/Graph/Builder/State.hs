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
import           Luna.Data.StructInfo            (StructInfo)
import qualified Luna.Data.StructInfo            as StructInfo
import           Luna.Pass.Pass                  (PassMonad)
import qualified Luna.Syntax.AST                 as AST
import           Luna.Syntax.Graph.Edge          (Edge)
import qualified Luna.Syntax.Graph.Edge          as Edge
import           Luna.Syntax.Graph.Flags         (Flags)
import qualified Luna.Syntax.Graph.Flags         as Flags
import           Luna.Syntax.Graph.Graph         (Graph)
import qualified Luna.Syntax.Graph.Graph         as Graph
import           Luna.Syntax.Graph.Node          (Node)
import qualified Luna.Syntax.Graph.Node          as Node
import           Luna.Syntax.Graph.Node.Position (Position)
import           Luna.Syntax.Graph.Port          (Port)
import           Luna.Syntax.Graph.PropertyMap   (PropertyMap)
import qualified Luna.Syntax.Graph.PropertyMap   as PropertyMap



logger :: Logger
logger = getLogger $(moduleName)


type NodeMap = Map AST.ID (Node.ID, Port)


data GBState a e = GBState { _graph        :: Graph a e
                           , _nodeMap      :: NodeMap
                           , _aa           :: StructInfo
                           , _propertyMap  :: PropertyMap a e
                           , _foldNodes    :: Bool
                           , _prevoiusNode :: Node.ID
                           } deriving (Show)

makeLenses ''GBState


type GBPass a e m result = Monad m => PassMonad (GBState a e) m result


make :: StructInfo -> PropertyMap a e -> Bool -> Node.ID -> GBState a e
make = GBState Graph.empty Map.empty


addToNodeMap :: AST.ID -> (Node.ID, Port) -> GBPass a e m ()
addToNodeMap k v = do nm <- getNodeMap
                      setNodeMap $ Map.insert k v nm


insNode :: (Node.ID, Position -> Node a e) -> GBPass a e m ()
insNode (nodeID, node) = do
    g   <- getGraph
    pos <- Maybe.fromMaybe (0,0) <$> getPosition nodeID
    setGraph $ Graph.insNode (nodeID, node pos) g


insNodeWithFlags :: (Node.ID, Position -> Node a e) -> Bool -> Bool -> GBPass a e m ()
insNodeWithFlags n@(nodeID, _) isFolded assignment = do
    insNode n
    when isFolded   $ modifyFlags (Flags.astFolded     .~ Just True) nodeID
    when assignment $ modifyFlags (Flags.astAssignment .~ Just True) nodeID


addNode :: AST.ID -> Port -> (Position -> Node a e) -> Bool -> Bool -> GBPass a e m ()
addNode astID outPort node isFolded assignment = do
    insNodeWithFlags (astID, node) isFolded assignment
    addToNodeMap astID (astID, outPort)


connectNodes :: Node.ID -> Node.ID -> Edge -> GBPass a e m ()
connectNodes srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge


connect :: AST.ID -> Node.ID -> Port -> GBPass a e m ()
connect srcID dstNID dstPort = do
    found             <- gvmNodeMapLookUp srcID
    (srcNID, srcPort) <- lift $ found <??> "Graph.Builder.State.connect : cannot find " ++ show srcID
    connectNodes srcNID dstNID $ Edge.Data srcPort dstPort


connectMonadic :: Node.ID -> GBPass a e m ()
connectMonadic nodeID = do
    prevID <- getPrevoiusNode
    setPrevoiusNode nodeID
    prevPos <- Maybe.fromMaybe (0,0) <$> getPosition prevID
    currPos <- getPosition nodeID
    when (Maybe.isNothing currPos) $
        setPosition nodeID (fst prevPos + 10, snd prevPos)
    connectNodes prevID nodeID Edge.Monadic


getGraph :: GBPass a e m (Graph a e)
getGraph = gets (view graph)


setGraph :: Graph a e -> GBPass a e m ()
setGraph gr = modify (set graph gr)


getNodeMap :: GBPass a e m NodeMap
getNodeMap = gets (view nodeMap)


setNodeMap :: NodeMap -> GBPass a e m ()
setNodeMap nm = modify (set nodeMap nm)


getAAMap :: GBPass a e m StructInfo
getAAMap = gets (view aa)


setAAMap :: StructInfo -> GBPass a e m ()
setAAMap aa' = modify (set aa aa')


getPropertyMap :: GBPass a e m (PropertyMap a e)
getPropertyMap = gets (view propertyMap)


setPropertyMap :: PropertyMap a e -> GBPass a e m ()
setPropertyMap pm = modify (set propertyMap pm)


aaLookUp :: AST.ID -> GBPass a e m (Maybe AST.ID)
aaLookUp astID = do aa' <- getAAMap
                    return $ view StructInfo.target
                          <$> IntMap.lookup astID (aa' ^. StructInfo.alias)


nodeMapLookUp :: AST.ID -> GBPass a e m (Maybe (Node.ID, Port))
nodeMapLookUp astID = do nm <- getNodeMap
                         return $ Map.lookup astID nm


gvmNodeMapLookUp :: AST.ID -> GBPass a e m (Maybe (Node.ID, Port))
gvmNodeMapLookUp astID = do
    found <- nodeMapLookUp =<< Maybe.fromMaybe astID <$> aaLookUp astID
    if Maybe.isNothing found
        then nodeMapLookUp astID
        else return found


getPrevoiusNode :: GBPass a e m Node.ID
getPrevoiusNode = gets (view prevoiusNode)


setPrevoiusNode :: Node.ID -> GBPass a e m ()
setPrevoiusNode nodeID = modify (set prevoiusNode nodeID)


modifyFlags :: (Flags -> Flags) -> Node.ID -> GBPass a e m ()
modifyFlags fun nodeID =
    getPropertyMap >>= setPropertyMap . PropertyMap.modifyFlags fun nodeID


getFlags :: Node.ID -> GBPass a e m Flags
getFlags nodeID = PropertyMap.getFlags nodeID <$> getPropertyMap


getPosition :: Node.ID -> GBPass a e m (Maybe Position)
getPosition nodeID =
    view Flags.nodePosition <$> getFlags nodeID


setPosition :: Node.ID -> Position -> GBPass a e m ()
setPosition nodeID pos = do
    modifyFlags (Flags.nodePosition .~ Just pos) nodeID
    graph' <- getGraph
    node   <- lift $ Graph.lab graph' nodeID <??> "BuilderState.setPosition : cannot find node with id = " ++ show nodeID
    setGraph $ Graph.updateNode (nodeID, node & Node.pos .~ pos) graph'


getDefaultGenerated :: Node.ID -> GBPass a e m Bool
getDefaultGenerated nodeID = do
    foldSetting <- gets (view foldNodes)
    if foldSetting
        then flip Flags.isSet' (view Flags.defaultNodeGenerated) <$> getFlags nodeID
        else return False


getGraphFolded :: Node.ID -> GBPass a e m Bool
getGraphFolded nodeID = do
    foldSetting <- gets (view foldNodes)
    if foldSetting
        then Flags.isFolded <$> getFlags nodeID
        else return False


setGrouped :: Node.ID -> GBPass a e m ()
setGrouped = modifyFlags (Flags.grouped .~ Just True)
