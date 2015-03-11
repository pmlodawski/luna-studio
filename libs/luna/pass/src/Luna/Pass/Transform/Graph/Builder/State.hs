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
import           Luna.Syntax.Graph.Graph         (Graph)
import qualified Luna.Syntax.Graph.Graph         as Graph
import           Luna.Syntax.Graph.Node          (Node)
import qualified Luna.Syntax.Graph.Node          as Node
import           Luna.Syntax.Graph.Node.Position (Position)
import           Luna.Syntax.Graph.Port          (Port)
import           Luna.Syntax.Graph.Tag           (TDecl, Tag)
import qualified Luna.Syntax.Graph.Tag           as Tag
import           Luna.Syntax.Label               (Label (Label))
import qualified Luna.Syntax.Label               as Label

getPosition = undefined
setPosition = undefined




logger :: Logger
logger = getLogger $moduleName


type NodeMap = Map Node.ID (Node.ID, Port)

type GBPass a e m result = Monad m => PassMonad (GBState a e) m result

data GBState a e = GBState { _graph          :: Graph a e
                           , _nodeMap        :: NodeMap

                           , _resetNodeIDs   :: Bool
                           , _nextFreeNodeID :: Node.ID

                           , _aa             :: StructInfo
                           , _foldNodes      :: Bool
                           , _prevoiusNode   :: Node.ID
                           } deriving (Show)

makeLenses ''GBState


make :: StructInfo -> Bool -> Node.ID -> GBState a e
make = GBState def def True def

-- == getters and setters ================================================
----- graph ---------------------------------------------------------------
getGraph :: GBPass a e m (Graph a e)
getGraph = gets $ view graph

setGraph :: Graph a e -> GBPass a e m ()
setGraph = modify . set graph

----- nodeMap -------------------------------------------------------------
getNodeMap :: GBPass a e m NodeMap
getNodeMap = gets $ view nodeMap

setNodeMap :: NodeMap -> GBPass a e m ()
setNodeMap = modify . set nodeMap

addToNodeMap :: AST.ID -> (Node.ID, Port) -> GBPass a e m ()
addToNodeMap k v = setNodeMap . Map.insert k v =<< getNodeMap

----- resetNodeIDs --------------------------------------------------------
setResetNodeIDs :: Bool -> GBPass a e m ()
setResetNodeIDs = modify . set resetNodeIDs

getResetNodeIDs :: GBPass a e m Bool
getResetNodeIDs = gets $ view resetNodeIDs

----- nextFreeNodeID ------------------------------------------------------
setNextFreeNodeID :: Node.ID -> GBPass a e m ()
setNextFreeNodeID = modify . set nextFreeNodeID

getNextFreeNodeID :: GBPass a e m Node.ID
getNextFreeNodeID = gets $ view nextFreeNodeID

initFreeNodeID :: TDecl v -> GBPass a e m ()
initFreeNodeID decl = case decl ^. Label.label of
    Tag.Empty {}        -> setResetNodeIDs True  >> setNextFreeNodeID 0
    Tag.Node _ nodeID _ -> setResetNodeIDs False >> setNextFreeNodeID nodeID

saveFreeNodeID :: TDecl v -> GBPass a e m (TDecl v)
saveFreeNodeID (Label tag decl) = do
    freeNodeID <- getNextFreeNodeID
    return (Label (Tag.mkNode freeNodeID def tag) decl)

getNodeInfo :: Label Tag e -> GBPass a e1 m (Node.ID, Position, Label Tag e)
getNodeInfo labeled@(Label tag e) = case tag of
    Tag.Node _ nodeID position -> return (nodeID, position, labeled)
    Tag.Empty {}        -> do
        freeNodeID <- getNextFreeNodeID
        setNextFreeNodeID $ freeNodeID + 1
        let pos  = def
            tag' = Tag.mkNode freeNodeID def tag
        return (freeNodeID, pos, Label tag' e)

---------------------------------------------------------------------------

insNode :: (Node.ID, Node a e) -> GBPass a e m ()
insNode (nodeID, node) =
    getGraph >>= setGraph . Graph.insNode (nodeID, node)


--insNodeWithFlags :: (Node.ID, Position -> Node a e) -> Bool -> Bool -> GBPass a e m ()
--insNodeWithFlags n@(nodeID, _) isFolded assignment = do
--    insNode n
--    when isFolded   $ modifyFlags (Flags.astFolded     .~ Just True) nodeID
--    when assignment $ modifyFlags (Flags.astAssignment .~ Just True) nodeID


--addNode :: AST.ID -> Port -> (Position -> Node a e) -> Bool -> Bool -> GBPass a e m ()
--addNode astID outPort node isFolded assignment = do
--    insNodeWithFlags (astID, node) isFolded assignment
--    addToNodeMap astID (astID, outPort)

connect :: Node.ID -> Node.ID -> Port -> GBPass a e m ()
connect srcID dstNID dstPort = do
    found             <- gvmNodeMapLookUp srcID
    (srcNID, srcPort) <- lift $ found <??> "Graph.Builder.State.connect : cannot find " ++ show srcID
    connectNodes srcNID dstNID $ Edge.Data srcPort dstPort

connectNodes :: Node.ID -> Node.ID -> Edge -> GBPass a e m ()
connectNodes srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge

connectMonadic :: Node.ID -> GBPass a e m ()
connectMonadic nodeID = do
    prevID <- getPrevoiusNode
    setPrevoiusNode nodeID
    prevPos <- Maybe.fromMaybe (0,0) <$> getPosition prevID
    currPos <- getPosition nodeID
    when (Maybe.isNothing currPos) $
        setPosition nodeID (fst prevPos + 10, snd prevPos)
    connectNodes prevID nodeID Edge.Monadic



getAAMap :: GBPass a e m StructInfo
getAAMap = gets $ view aa


setAAMap :: StructInfo -> GBPass a e m ()
setAAMap = modify . set aa


aaLookUp :: AST.ID -> GBPass a e m (Maybe AST.ID)
aaLookUp astID = do
    aa' <- getAAMap
    return $ view StructInfo.target
          <$> IntMap.lookup astID (aa' ^. StructInfo.alias)


nodeMapLookUp :: Node.ID -> GBPass a e m (Maybe (Node.ID, Port))
nodeMapLookUp nodeID = Map.lookup nodeID <$> getNodeMap


gvmNodeMapLookUp :: Node.ID -> GBPass a e m (Maybe (Node.ID, Port))
gvmNodeMapLookUp nodeID = do
    found <- nodeMapLookUp =<< Maybe.fromMaybe nodeID <$> aaLookUp nodeID
    if Maybe.isNothing found
        then nodeMapLookUp nodeID
        else return found


getPrevoiusNode :: GBPass a e m Node.ID
getPrevoiusNode = gets $ view prevoiusNode


setPrevoiusNode :: Node.ID -> GBPass a e m ()
setPrevoiusNode = modify . set prevoiusNode


--modifyFlags :: (Flags -> Flags) -> Node.ID -> GBPass a e m ()
--modifyFlags fun nodeID =
--    getPropertyMap >>= setPropertyMap . PropertyMap.modifyFlags fun nodeID


--getFlags :: Node.ID -> GBPass a e m Flags
--getFlags nodeID = PropertyMap.getFlags nodeID <$> getPropertyMap


--getPosition :: Node.ID -> GBPass a e m (Maybe Position)
--getPosition nodeID =
--    view Flags.nodePosition <$> getFlags nodeID


--setPosition :: Node.ID -> Position -> GBPass a e m ()
--setPosition nodeID pos = do
--    modifyFlags (Flags.nodePosition .~ Just pos) nodeID
--    graph' <- getGraph
--    node   <- lift $ Graph.lab graph' nodeID <??> "BuilderState.setPosition : cannot find node with id = " ++ show nodeID
--    setGraph $ Graph.updateNode (nodeID, node & Node.pos .~ pos) graph'


--getDefaultGenerated :: Node.ID -> GBPass a e m Bool
--getDefaultGenerated nodeID = do
--    foldSetting <- gets (view foldNodes)
--    if foldSetting
--        then flip Flags.isSet' (view Flags.defaultNodeGenerated) <$> getFlags nodeID
--        else return False


--getGraphFolded :: Node.ID -> GBPass a e m Bool
--getGraphFolded nodeID = do
--    foldSetting <- gets (view foldNodes)
--    if foldSetting
--        then Flags.isFolded <$> getFlags nodeID
--        else return False


--setGrouped :: Node.ID -> GBPass a e m ()
--setGrouped = modifyFlags (Flags.grouped .~ Just True)
