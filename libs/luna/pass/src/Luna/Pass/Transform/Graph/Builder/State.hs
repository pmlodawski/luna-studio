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

import           Control.Monad.State              hiding (mapM_)
import qualified Data.IntMap                      as IntMap
import qualified Data.IntSet                      as IntSet
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe

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
import           Luna.Syntax.Graph.Tag            (TDecl, TExpr, Tag)
import qualified Luna.Syntax.Graph.Tag            as Tag
import           Luna.Syntax.Label                (Label (Label))
import qualified Luna.Syntax.Label                as Label



logger :: Logger
logger = getLogger $moduleName


type NodeMap = Map AST.ID (Node.ID, SrcPort)
type Error = String

type GBPass v m result = Monad m => StateT (GBState v) (EitherT Error m) result

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

insNode :: (Node.ID, Node Tag v) -> GBPass v m ()
insNode (nodeID, node) =
    getGraph >>= setGraph . Graph.insNode (nodeID, node)

connect :: Node.ID -> SrcPort -> Node.ID -> DstPort -> GBPass v m ()
connect srcNID srcPort dstNID dstPort =
    connectNodes srcNID dstNID $ Edge.Data srcPort dstPort

connectNodes :: Node.ID -> Node.ID -> Edge -> GBPass v m ()
connectNodes srcID dstID edge = getGraph >>= setGraph . Graph.connect srcID dstID edge

connectMonadic :: Node.ID -> TExpr v -> GBPass v m (TExpr v)
connectMonadic nodeID lexpr = do
    newPos <- connectMonadic' nodeID
    return (lexpr & Label.label . Tag.position .~ newPos)

connectMonadic' :: Node.ID -> GBPass v m Position
connectMonadic' nodeID = do
    prevID   <- getPrevoiusNodeID
    prevNode <- getNode prevID
    currNode <- getNode nodeID
    let prevPos = prevNode ^. Node.pos
        currPos = currNode ^. Node.pos
        newPos  = if currPos == def
            then (fst prevPos + 10, snd prevPos)
            else currPos
    setPrevoiusNodeID nodeID
    connectNodes prevID nodeID Edge.Monadic
    updateNode (nodeID, currNode & Node.pos .~ newPos)
    return newPos

connectMonadicOutput :: TDecl v -> GBPass v m (TDecl v)
connectMonadicOutput ldecl = do
    newPos <- connectMonadic' Node.outputID
    return (ldecl & Label.label . Tag.additionalPos .~ Just newPos)

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
    mapM_ (`addToNodeMap` np) $ IntSet.toList ids

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

initFreeNodeID :: TDecl v -> GBPass v m (Position, Position, TDecl v)
initFreeNodeID decl = case decl ^. Label.label of
    Tag.Node _ freeNodeID inputsPos mOutputPos _ -> do
        let outputPos = Maybe.fromMaybe def mOutputPos
        setResetNodeIDs False
        setNextFreeNodeID freeNodeID
        return (inputsPos, outputPos, decl & Label.label . Tag.additionalPos .~ Just outputPos)
    Tag.Empty {} -> do
        setResetNodeIDs True
        return (def, def, decl & Label.label %~ Tag.mkNode def def (Just def))

saveFreeNodeID :: TDecl v -> GBPass v m (TDecl v)
saveFreeNodeID decl = do
    freeNodeID <- getNextFreeNodeID
    return (decl & Label.label . Tag.nodeID .~ freeNodeID)

getNodeInfo :: Label Tag e -> GBPass e1 m (Node.ID, Position, Label Tag e)
getNodeInfo labeled@(Label tag e) = case tag of --FIXME[PM] when ResetNodeIDs set, always assign new nodeID
    Tag.Node _ nodeID position _ _ -> return (nodeID, position, labeled)
    Tag.Empty {}        -> do
        freeNodeID <- getNextFreeNodeID
        setNextFreeNodeID $ freeNodeID + 1
        let pos  = def
            tag' = Tag.mkNode freeNodeID def def tag
        return (freeNodeID, pos, Label tag' e)

----- aa ------------------------------------------------------------------
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

gvmNodeMapLookUp :: AST.ID -> GBPass v m (Maybe (Node.ID, SrcPort))
gvmNodeMapLookUp astID =
    nodeMapLookUp =<< Maybe.fromMaybe astID <$> aaLookUp astID

----- previousNodeID ------------------------------------------------------
getPrevoiusNodeID :: GBPass v m Node.ID
getPrevoiusNodeID = gets $ view prevoiusNodeID

setPrevoiusNodeID :: Node.ID -> GBPass v m ()
setPrevoiusNodeID = modify . set prevoiusNodeID
