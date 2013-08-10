---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Graph (
    Graph,
    Vertex,
    LVertex,
    Edge,
    empty,
    delNode,
    gelem, -- Graph gr => Node -> gr a b -> BoolSource  -  True if the Node is present in the Graph.
    insNode,
    insNodes,
    updateNode,
    insEdge,
    insEdges,
    lab,
    lab_deprecated,
    labEdges,
    labNodes,
    labs,
    labVtx,
    labVtxs,
    mkGraph,
    newNodes,
    out,
    out_,
    inn,
    inn_,
    innvtx,
    suc,
    suc_,
    sucl,
    pre,
    pre_,
    prel,
    topsort,
    path,
    newVtxs,
    newVtx
    --newIds
) where

import qualified Data.Graph.Inductive            as DG
import           Data.Graph.Inductive            hiding (Graph)


type Graph a b = DG.Gr a b
type Vertex    = DG.Node
type LVertex a = DG.LNode a


lab_deprecated :: Graph a b -> Vertex -> a
lab_deprecated g vtx = DG.lab' $ DG.context g vtx


labs :: Graph a b -> [Vertex] -> [a]
labs g vtxs = map (lab_deprecated g) vtxs


labVtx :: Graph a b -> Vertex -> LVertex a
labVtx g vtx = (vtx, lab_deprecated g vtx)


labVtxs :: Graph a b -> [Vertex] -> [LVertex a]
labVtxs g vtxs = map (labVtx g) vtxs


out_ :: Graph a b -> Vertex -> [b]
out_ g vtx = [el | (_,_,el) <- out g vtx]


inn_ :: Graph a b -> Vertex -> [b]
inn_ g vtx = [el | (_,_,el) <- inn g vtx]


innvtx :: Graph a b -> Vertex -> [Vertex]
innvtx g vtx = [pvtx | (pvtx,_,_) <- inn g vtx]


suc_ :: Graph a b -> Vertex -> [a]
suc_ g vtx = map (lab_deprecated g) $ suc g vtx


sucl :: Graph a b -> Vertex -> [(Vertex, a)]
sucl g vtx = map (\v -> (v, lab_deprecated g v)) $ suc g vtx


pre_ :: Graph a b -> Vertex -> [a]
pre_ g vtx = map (lab_deprecated g) $ pre g vtx


prel :: Graph a b -> Vertex -> [(Vertex, a)]
prel g vtx = map (\v -> (v, lab_deprecated g v)) $ pre g vtx


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