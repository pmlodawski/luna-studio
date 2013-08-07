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
import qualified Luna.Network.Def.Definition     as Definition
import           Luna.Network.Def.Definition       (Definition(..))
import           Luna.Network.Def.Edge             (Edge(..))

import           Luna.Data.Graph                                   hiding(Edge)
import           Luna.Data.List                    (foldri)

type DefManager = Graph Definition Edge


addToParent :: (Definition.ID, Definition.ID, Definition) -> DefManager -> DefManager
addToParent (parentID, defID, def) manager = insEdge (parentID, defID, Edge) $
                                             insNode (defID, def) manager

addToParentMany :: [(Definition.ID, Definition.ID, Definition)] -> DefManager -> DefManager
addToParentMany = foldri addToParent

pathNames :: DefManager -> Definition.ID -> [String]
pathNames g vtx = fmap (Type.name . Definition.cls . (lab_deprecated g)) $ path g vtx


children :: DefManager -> Definition.ID -> [(Definition.ID, Definition)]
children = sucl

parent :: DefManager -> Definition.ID -> Maybe (Definition.ID, Definition)
parent defManager defID = case prel defManager defID of 
    [] -> Nothing
    [a] -> Just a
    _ -> error $ (show defID) ++ " has multiple parents!"