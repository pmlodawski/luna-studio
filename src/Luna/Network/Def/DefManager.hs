---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Def.DefManager(
    DefManager(..),
    
    empty,
    
    add,         addMany,
    addToParent, addToParentMany
) where

import           Luna.Data.List             (foldri)
import qualified Data.Graph.Inductive     as DG
import           Luna.Network.Def.NodeDef   (NodeDef)
import qualified Luna.Network.Def.Edge    as Edge
import           Luna.Network.Def.Edge      (Edge(..))


data DefManager = DefManager{
    repr      :: DG.Gr NodeDef Edge
} deriving (Show)

empty :: DefManager
empty = DefManager DG.empty


add :: DG.LNode NodeDef -> DefManager -> DefManager
add def manager = manager {repr = DG.insNode def $ repr manager}

addMany :: [DG.LNode NodeDef] -> DefManager -> DefManager
addMany = foldri add

addToParent :: (DG.Node, DG.Node, NodeDef) -> DefManager -> DefManager
addToParent (parentID, nodeID, def) manager = 
    manager {repr = DG.insEdge (parentID, nodeID, Edge.Contain) $
                    DG.insNode (nodeID, def) $ repr manager}

addToParentMany :: [(DG.Node, DG.Node, NodeDef)] -> DefManager -> DefManager
addToParentMany = foldri addToParent


--class Graph gr a b where
--  empty' :: DG.Gr a b
--  empty' = DG.empty

--  repr' :: gr -> DG.Gr a b

--  add' :: a -> gr -> gr
--  add' el g = g{repr' = DG.insNode el $ repr' g}
    --repr'  :: gr a b -> DG.Gr a b
    --add'   :: a -> gr a b -> gr a b


--instance Graph DefManager where
--  empty' = DefManager DG.empty