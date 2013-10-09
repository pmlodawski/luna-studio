---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Def.DefManager(
    module Flowbox.Data.Graph,
    DefManager,
    addToParent,
    addToParentMany,
    addNewToParent,
    delete,
    pathNames,
    children,
    parent,
    empty
) where

import           Flowbox.Prelude                     hiding (fromJust)
import           Data.Maybe                            (fromJust)

import qualified Flowbox.Luna.XOLD.Type.Type         as Type
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition)
import           Flowbox.Luna.Network.Def.Edge         (Edge(Edge))

import           Flowbox.Data.Graph                  hiding (Graph, Edge, empty)
import qualified Flowbox.Data.Graph                  as DG
import           Flowbox.Data.List                     (foldri)

type DefManager = DG.Graph Definition Edge


empty :: DefManager
empty = DG.empty


addToParent :: (Definition.ID, Definition.ID, Definition) -> DefManager -> DefManager
addToParent (parentID, defID, def) manager = insEdge (parentID, defID, Edge) $
                                             insNode (defID, def) manager

addToParentMany :: [(Definition.ID, Definition.ID, Definition)] -> DefManager -> DefManager
addToParentMany = foldri addToParent


addNewToParent :: (Definition.ID, Definition) -> DefManager -> (DefManager, Definition.ID)
addNewToParent (parentID, def) manager = (newDefManager, defID) where
    (manager', defID) = insNewNode def manager
    newDefManager = insEdge (parentID, defID, Edge) manager'


delete :: Definition.ID -> DefManager ->  DefManager
delete defID defManager = newDefManager where
    defManager' = foldr (delete) defManager $ suc defManager defID 
    newDefManager = delNode defID defManager'


pathNames :: DefManager -> Definition.ID -> [String]
pathNames g vtx = fmap (Type.name . Definition.cls . fromJust . (lab g)) $ path g vtx


children :: DefManager -> Definition.ID -> [(Definition.ID, Definition)]
children = sucl


parent :: DefManager -> Definition.ID -> Maybe (Definition.ID, Definition)
parent defManager defID = case prel defManager defID of 
    [] -> Nothing
    [a] -> Just a
    _ -> error $ (show defID) ++ " has multiple parents!"
