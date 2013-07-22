---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Graph.Graph(
    Graph(..),
    empty,

    --insFreshNode, 
    add,        addMany, 
    delete,     deleteMany,
    connect,    connectMany,
    disconnect, disconnectMany,

    lnodeById, nodeById, 
) where


import qualified Data.Graph.Inductive    as DG

import qualified Luna.Common.Graph       as CommonG
import           Luna.Data.List            (foldri)
import           Luna.Network.Graph.Edge   (Edge)
import           Luna.Network.Graph.Node   (Node)


data Graph = Graph {
    repr      :: DG.Gr Node Edge
} deriving (Show)


empty :: Graph
empty = Graph DG.empty

------------------------- EDITION ---------------------------

addMany :: [DG.LNode Node] -> Graph -> Graph
addMany = foldri add

add :: DG.LNode Node -> Graph -> Graph
add lnode graph = graph{repr=DG.insNode lnode $ repr graph}

-- deprecated
freshNodeID :: Graph -> Int 
freshNodeID gr = case DG.nodes $ repr gr of
                   []       -> 0
                   nodeList -> 1 + maximum nodeList

-- deprecated
insFreshNode :: Node -> Graph -> Graph
insFreshNode node gr = add ((freshNodeID gr), node) gr 


deleteMany :: [DG.Node] -> Graph -> Graph
deleteMany = foldri delete

delete :: DG.Node -> Graph -> Graph
delete id_ graph =
    graph{repr=DG.delNode id_ $ repr graph }

connectMany :: [DG.LEdge Edge] -> Graph -> Graph
connectMany = foldri connect 

connect :: DG.LEdge Edge -> Graph -> Graph
connect ledge graph = graph{repr = DG.insEdge ledge $ repr graph}


disconnectMany :: [DG.Edge] -> Graph -> Graph
disconnectMany = foldri disconnect 

disconnect :: DG.Edge -> Graph -> Graph
disconnect edge graph = graph{repr = DG.delEdge edge $ repr graph}

------------------------- GETTERS ---------------------------

lnodeById :: Graph -> DG.Node -> DG.LNode Node
lnodeById graph nid = CommonG.lnodeById (repr graph) nid

nodeById :: Graph -> DG.Node -> Node
nodeById graph nid = CommonG.nodeById (repr graph) nid

------------------------- INSTANCES -------------------------

--instance Serialize Graph where
--  put i = Serialize.put (repr i, children i, types i, calls i, classes i, functions i, packages i)
--  get   = do 
--            (repr', children', types', calls', classes', functions', packages') <- Serialize.get
--            return $ Graph repr' children' types' calls' classes' functions' packages'


--instance (Serialize a, Serialize b) => Serialize (DG.Gr a b) where
--  put i = Serialize.put (DG.labNodes i, DG.labEdges i)
--  get = do
--          (nd, edg) <- Serialize.get
--          return $ DG.mkGraph nd edg


---- FIXME[wd] move the following instance to the right place
--instance (Show k, Show a) => Show (MultiMap k a) where
--    show a = show $ MultiMap.toMap a