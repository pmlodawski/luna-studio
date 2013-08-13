---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Graph (
    module Data.Graph.Inductive,
    Graph,
    Vertex,
    LVertex,

    updateNode,
    labs,
    labVtx,
    labVtxs,
    out_,
    inn_,
    innvtx,
    suc_,
    sucl,
    pre_,
    prel,
    topsort,
    path,
    newVtxs,
    newVtx,
    insNewNode,
    --newIds
) where

import           Data.Functor           ((<$>))
import           Data.Maybe             (fromJust)
import qualified Data.Graph.Inductive as DG
import           Data.Graph.Inductive                       hiding (Node, Graph)


type Graph a b = DG.Gr a b
type Vertex    = DG.Node
type LVertex a = DG.LNode a


labs :: Graph a b -> [Vertex] -> Maybe [a]
labs g vtxs = mapM (lab g) vtxs


labVtx :: Graph a b -> Vertex -> Maybe (LVertex a)
labVtx g vtx = (,) vtx <$> lab g vtx


labVtxs :: Graph a b -> [Vertex] -> Maybe [LVertex a]
labVtxs g vtxs = mapM (labVtx g) vtxs


out_ :: Graph a b -> Vertex -> [b]
out_ g vtx = [el | (_,_,el) <- out g vtx]


inn_ :: Graph a b -> Vertex -> [b]
inn_ g vtx = [el | (_,_,el) <- inn g vtx]


innvtx :: Graph a b -> Vertex -> [Vertex]
innvtx g vtx = [pvtx | (pvtx,_,_) <- inn g vtx]


suc_ :: Graph a b -> Vertex -> [a]
suc_ g vtx = map (fromJust . (lab g)) $ suc g vtx


sucl :: Graph a b -> Vertex -> [LVertex a]
sucl g vtx = map (fromJust . (labVtx g)) $ suc g vtx


pre_ :: Graph a b -> Vertex -> [a]
pre_ g vtx = map (fromJust . (lab g)) $ pre g vtx


prel :: Graph a b -> Vertex -> [LVertex a]
prel g vtx = map (fromJust . (labVtx g)) $ pre g vtx


path :: Graph a b -> Vertex -> [Vertex]
path g vtx = case pre g vtx of
    []       -> [vtx]
    [parent] -> path g parent ++ [vtx]
    _        -> error "Node has multiple parents"


updateNode :: LVertex a -> Graph a b -> Graph a b
updateNode (vid, v) graph = newGraph where 
    (c_ins, c_id, _, c_outs) = context graph vid
    newContext = (c_ins, c_id, v, c_outs)
    newGraph = newContext & delNode vid graph


newVtxs :: Graph a b -> [Vertex]
newVtxs g = [n+1..] where (_,n) = nodeRange g


newVtx :: Graph a b -> Vertex
newVtx = head . newVtxs


insNewNode :: a -> Graph a b -> (Graph a b, Vertex)
insNewNode node graph = (newGraph, nodeID) where
    nodeID   = newVtx graph
    newGraph = insNode (nodeID, node) graph
