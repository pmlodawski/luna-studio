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
    addToParent, addToParentMany,
    pathOf,nodeById
) where

import qualified Luna.Common.Graph        as CommonG
import           Luna.Data.List             (foldri)
import qualified Data.Graph.Inductive     as DG
import qualified Luna.Network.Def.NodeDef as NodeDef
import           Luna.Network.Def.NodeDef   (NodeDef(..))
import qualified Luna.Network.Def.Edge    as Edge
import           Luna.Network.Def.Edge      (Edge(..))
import qualified Luna.Network.Path.Path   as Path
import           Luna.Network.Path.Path     (Path(..))
import qualified Luna.Type.Type           as Type

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

pathOf :: DG.Node -> DefManager -> Path
pathOf nid manager = case DG.pre (repr manager) nid of
    []       -> Path [name]
    [parent] -> Path.append name $ pathOf parent manager
    _        -> error "Node has multiple parents"
    where name = Type.name $ NodeDef.cls $ nodeById manager nid


nodeById :: DefManager -> DG.Node -> NodeDef
nodeById manager = CommonG.nodeById (repr manager)
 -- -- 

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