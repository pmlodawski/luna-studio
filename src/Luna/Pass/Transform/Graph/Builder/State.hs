---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Luna.Pass.Transform.Graph.Builder.State where

import           Control.Monad.State hiding (mapM_)
import qualified Data.IntMap         as IntMap
import qualified Data.IntSet         as IntSet
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Luna.Data.StructInfo             (StructInfo)
import qualified Luna.Data.StructInfo             as StructInfo
import qualified Luna.Pass.Analysis.ID.ExtractIDs as ExtractIDs
import qualified Luna.Syntax.AST                  as AST
import           Luna.Syntax.Graph.Edge           (Edge)
import qualified Luna.Syntax.Graph.Edge           as Edge
import           Luna.Syntax.Graph.Graph          (Graph)
import qualified Luna.Syntax.Graph.Graph          as Graph
import           Luna.Syntax.Graph.Node           (Node)
import qualified Luna.Syntax.Graph.Node           as Node
import           Luna.Syntax.Graph.Node.Position  (Position)
import           Luna.Syntax.Graph.Port           (DstPort, SrcPort)
import           Luna.Syntax.Graph.Tag            (TDecl, Tag)
import qualified Luna.Syntax.Graph.Tag            as Tag
import           Luna.Syntax.Label                (Label (Label))
import qualified Luna.Syntax.Label                as Label



logger :: Logger
logger = getLogger $moduleName


type NodeMap = Map AST.ID (Node.ID, SrcPort)
type Error = String

type GBPass v m result = Monad m => StateT (GBState v) (EitherT String m) result

data GBState v = GBState { _graph          :: Graph Tag v
                         , _nodeMap        :: NodeMap

                         , _resetNodeIDs   :: Bool
                         , _nextFreeNodeID :: Node.ID

                         , _aa             :: StructInfo
                         , _prevoiusNodeID :: Node.ID
                         } deriving (Show)

makeLenses ''GBState


mk :: StructInfo -> Node.ID -> GBState e
mk = GBState def def True def

-- == getters and setters ================================================
----- graph ---------------------------------------------------------------
getGraph :: GBPass v m (Graph Tag v)
getGraph = gets $ view graph

setGraph :: Graph Tag v -> GBPass v m ()
setGraph = modify . set graph

getNode :: Node.ID -> GBPass v m (Node Tag v)
getNode nodeID = do
    gr   <- getGraph
    lift $ Graph.lab gr nodeID <??> "addNodeDefault : Cannot find nodeID = " ++ show nodeID

updateNode :: (Node.ID, Node Tag v) -> GBPass v m ()
updateNode node = getGraph >>= setGraph . Graph.updateNode node


----- nodeMap -------------------------------------------------------------
getNodeMap :: GBPass v m NodeMap
getNodeMap = gets $ view nodeMap

setNodeMap :: NodeMap -> GBPass v m ()
setNodeMap = modify . set nodeMap

addToNodeMap :: AST.ID -> (Node.ID, SrcPort) -> GBPass v m ()
addToNodeMap k v = setNodeMap . Map.insert k v =<< getNodeMap


registerIDs :: ExtractIDs.EIDDefaultTraversal Identity labeled
            => labeled -> (Node.ID, SrcPort) -> GBPass v m ()
registerIDs labeled np = do
    let ids = ExtractIDs.run labeled
    mapM_ (flip addToNodeMap np) $ IntSet.toList ids


----- resetNodeIDs --------------------------------------------------------
setResetNodeIDs :: Bool -> GBPass v m ()
setResetNodeIDs = modify . set resetNodeIDs

getResetNodeIDs :: GBPass v m Bool
getResetNodeIDs = gets $ view resetNodeIDs

----- nextFreeNodeID ------------------------------------------------------
setNextFreeNodeID :: Node.ID -> GBPass v m ()
setNextFreeNodeID = modify . set nextFreeNodeID

getNextFreeNodeID :: GBPass v m Node.ID
getNextFreeNodeID = gets $ view nextFreeNodeID

initFreeNodeID :: TDecl v -> GBPass v m ()
initFreeNodeID decl = case decl ^. Label.label of
    Tag.Empty {}        -> setResetNodeIDs True  >> setNextFreeNodeID 0
    Tag.Node _ nodeID _ -> setResetNodeIDs False >> setNextFreeNodeID nodeID

saveFreeNodeID :: TDecl v -> GBPass v m (TDecl v)
saveFreeNodeID (Label tag decl) = do
    freeNodeID <- getNextFreeNodeID
    return (Label (Tag.mkNode freeNodeID def tag) decl)

getNodeInfo :: Label Tag e -> GBPass e1 m (Node.ID, Position, Label Tag e)
getNodeInfo labeled@(Label tag e) = case tag of
    Tag.Node _ nodeID position -> return (nodeID, position, labeled)
    Tag.Empty {}        -> do
        freeNodeID <- getNextFreeNodeID
        setNextFreeNodeID $ freeNodeID + 1
        let pos  = def
            tag' = Tag.mkNode freeNodeID def tag
        return (freeNodeID, pos, Label tag' e)

---------------------------------------------------------------------------

insNode :: (Node.ID, Node Tag v) -> GBPass v m ()
insNode (nodeID, node) =
    getGraph >>= setGraph . Graph.insNode (nodeID, node)

--addNodeDefault :: Node.ID -> PortDescriptor -> NodeExpr a v -> GBPass v m ()
--addNodeDefault nodeID pd ne = do
--    gr   <- getGraph
--    node <- lift $ Graph.lab gr nodeID <??> "addNodeDefault : Cannot find nodeID = " ++ show nodeID
--    let node' = Node.insertDefault pd ne node
--    setGraph $ Graph.insNode (nodeID, node') gr

--insNodeWithFlags :: (Node.ID, Position -> Node a v) -> Bool -> Bool -> GBPass v m ()
--insNodeWithFlags n@(nodeID, _) isFolded assignment = do
--    insNode n
--    when isFolded   $ modifyFlags (Flags.astFolded     .~ Just True) nodeID
--    when assignment $ modifyFlags (Flags.astAssignment .~ Just True) nodeID


--addNode :: AST.ID -> Port -> (Position -> Node a v) -> Bool -> Bool -> GBPass v m ()
--addNode astID outPort node isFolded assignment = do
--    insNodeWithFlags (astID, node) isFolded assignment
--    addToNodeMap astID (astID, outPort)

connect :: Node.ID -> SrcPort -> Node.ID -> DstPort -> GBPass v m ()
connect srcNID srcPort dstNID dstPort =
    connectNodes srcNID dstNID $ Edge.Data srcPort dstPort

connectNodes :: Node.ID -> Node.ID -> Edge -> GBPass v m ()
connectNodes srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge

connectMonadic :: Node.ID -> GBPass v m ()
connectMonadic nodeID = do
    prevID   <- getPrevoiusNodeID
    prevNode <- getNode prevID
    currNode <- getNode nodeID
    setPrevoiusNodeID nodeID
    let prevPos = prevNode ^. Node.pos
        currPos = currNode ^. Node.pos
    when (prevPos == currPos) $
        updateNode (nodeID, currNode & Node.pos .~ (fst prevPos + 10, snd prevPos))
    connectNodes prevID nodeID Edge.Monadic


getAAMap :: GBPass v m StructInfo
getAAMap = gets $ view aa


setAAMap :: StructInfo -> GBPass v m ()
setAAMap = modify . set aa


aaLookUp :: AST.ID -> GBPass v m (Maybe AST.ID)
aaLookUp astID = do
    aa' <- getAAMap
    return $ view StructInfo.target
          <$> IntMap.lookup astID (aa' ^. StructInfo.alias)


nodeMapLookUp :: Node.ID -> GBPass v m (Maybe (Node.ID, SrcPort))
nodeMapLookUp nodeID = Map.lookup nodeID <$> getNodeMap


gvmNodeMapLookUp :: Node.ID -> GBPass v m (Maybe (Node.ID, SrcPort))
gvmNodeMapLookUp nodeID = do
    found <- nodeMapLookUp =<< Maybe.fromMaybe nodeID <$> aaLookUp nodeID
    if Maybe.isNothing found
        then nodeMapLookUp nodeID
        else return found


getPrevoiusNodeID :: GBPass v m Node.ID
getPrevoiusNodeID = gets $ view prevoiusNodeID


setPrevoiusNodeID :: Node.ID -> GBPass v m ()
setPrevoiusNodeID = modify . set prevoiusNodeID

--modifyFlags :: (Flags -> Flags) -> Node.ID -> GBPass v m ()
--modifyFlags fun nodeID =
--    getPropertyMap >>= setPropertyMap . PropertyMap.modifyFlags fun nodeID


--getFlags :: Node.ID -> GBPass v m Flags
--getFlags nodeID = PropertyMap.getFlags nodeID <$> getPropertyMap


--getPosition :: Node.ID -> GBPass v m (Maybe Position)
--getPosition nodeID =
--    view Flags.nodePosition <$> getFlags nodeID


--setPosition :: Node.ID -> Position -> GBPass v m ()
--setPosition nodeID pos = do
--    modifyFlags (Flags.nodePosition .~ Just pos) nodeID
--    graph' <- getGraph
--    node   <- lift $ Graph.lab graph' nodeID <??> "BuilderState.setPosition : cannot find node with id = " ++ show nodeID
--    setGraph $ Graph.updateNode (nodeID, node & Node.pos .~ pos) graph'


--getDefaultGenerated :: Node.ID -> GBPass v m Bool
--getDefaultGenerated nodeID = do
--    foldSetting <- gets (view foldNodes)
--    if foldSetting
--        then flip Flags.isSet' (view Flags.defaultNodeGenerated) <$> getFlags nodeID
--        else return False


--getGraphFolded :: Node.ID -> GBPass v m Bool
--getGraphFolded nodeID = do
--    foldSetting <- gets (view foldNodes)
--    if foldSetting
--        then Flags.isFolded <$> getFlags nodeID
--        else return False


--setGrouped :: Node.ID -> GBPass v m ()
--setGrouped = modifyFlags (Flags.grouped .~ Just True)
