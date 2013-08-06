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
    pathNames,
    children,
    parent
) where

import qualified Luna.Type.Type                  as Type
import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef(..))
import           Luna.Network.Def.Edge             (Edge(..))

import           Luna.Data.Graph                                   hiding(Edge)
import           Luna.Data.List                    (foldri)

type DefManager = Graph NodeDef Edge


addToParent :: (NodeDef.ID, NodeDef.ID, NodeDef) -> DefManager -> DefManager
addToParent (parentID, defID, def) manager = insEdge (parentID, defID, Edge) $
                                             insNode (defID, def) manager

addToParentMany :: [(NodeDef.ID, NodeDef.ID, NodeDef)] -> DefManager -> DefManager
addToParentMany = foldri addToParent

pathNames :: DefManager -> NodeDef.ID -> [String]
pathNames g vtx = fmap (Type.name . NodeDef.cls . (lab_deprecated g)) $ path g vtx


children :: DefManager -> NodeDef.ID -> [(NodeDef.ID, NodeDef)]
children = sucl

parent :: DefManager -> NodeDef.ID -> Maybe (NodeDef.ID, NodeDef)
parent defManager defID = case prel defManager defID of 
    [] -> Nothing
    [a] -> Just a
    _ -> error $ (show defID) ++ " has multiple parents!"