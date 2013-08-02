---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Def.DefManager(
    module Luna.Data.Graph,
    DefManager,
    addToParent,
    addToParentMany,
    pathNames
) where

import qualified Luna.Type.Type                  as Type
import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef(..))
import           Luna.Network.Def.Edge             (Edge(..))

import           Luna.Data.Graph                                   hiding(Edge)
import           Luna.Data.List                    (foldri)

type DefManager = Graph NodeDef Edge


addToParent :: (Vertex, Vertex, NodeDef) -> DefManager -> DefManager
addToParent (parentID, nodeID, def) manager = insEdge (parentID, nodeID, Edge) $
                                              insNode (nodeID, def) manager

addToParentMany :: [(Vertex, Vertex, NodeDef)] -> DefManager -> DefManager
addToParentMany = foldri addToParent


pathNames :: DefManager -> Vertex -> [String]
pathNames g vtx = fmap (Type.name . NodeDef.cls . (lab_deprecated g)) $ path g vtx
