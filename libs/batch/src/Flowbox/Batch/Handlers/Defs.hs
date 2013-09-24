---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Defs (
    defsGraph,
    defByID,
    
    addDefinition,
    updateDefinition ,
    removeDefinition,
    definitionChildren,
    definitionParent,
    resolveDefinition,
) where

import           Flowbox.Prelude                       
import           Flowbox.Batch.Batch                   (Batch(..))
import           Flowbox.Batch.Handlers.Common         (noresult, readonly, libManagerOp, defManagerOp, definitionOp)
import qualified Flowbox.Batch.Project.Project       as Project
import           Flowbox.Control.Error                 ((<?>), ifnot)
import qualified Flowbox.Luna.Lib.Library            as Library
import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition)
import qualified Flowbox.Luna.Network.Graph.Graph    as Graph
import qualified Flowbox.Luna.XOLD.Type.Type         as Type
import qualified Flowbox.Luna.Lib.NameResolver       as NameResolver



defsGraph :: Library.ID -> Project.ID -> Batch -> Either String DefManager
defsGraph libID projectID = readonly . defManagerOp libID projectID (\_ defManager -> do
    return (defManager, defManager))


defByID :: Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Definition
defByID defID libID projectID = readonly . definitionOp defID libID projectID (\_ def -> do
    return (def, def))


addDefinition :: Definition -> Definition.ID 
              -> Library.ID -> Project.ID -> Batch -> Either String (Batch, Definition.ID)
addDefinition definition parentID libID projectID = defManagerOp libID projectID (\_ defManager -> do
    DefManager.gelem parentID defManager `ifnot` ("Wrong 'parentID' = " ++ show parentID)
    let cls   = Definition.cls definition
        graph = case cls of Type.Function {} -> Graph.make
                            _                -> Graph.empty
        definitionWithGraph = definition { Definition.graph = graph }
    return $ DefManager.addNewToParent (parentID, definitionWithGraph) defManager)


updateDefinition :: (Definition.ID, Definition) 
                 -> Library.ID -> Project.ID -> Batch -> Either String Batch
updateDefinition (defID, def) libID projectID = noresult . definitionOp defID libID projectID (\_ oldDef -> do
    let agraph = Definition.graph oldDef
        newDef = def{Definition.graph = agraph}
    return (newDef, ()))


removeDefinition :: Definition.ID 
                 -> Library.ID -> Project.ID -> Batch -> Either String Batch 
removeDefinition defID libID projectID = noresult . defManagerOp libID projectID (\_ defManager -> do
    DefManager.gelem defID defManager `ifnot` ("Wrong 'defID' = " ++ show defID)
    let newDefManager = DefManager.delete defID defManager
    return (newDefManager, ()))


definitionChildren :: Definition.ID 
                   -> Library.ID -> Project.ID -> Batch -> Either String [(Definition.ID, Definition)]
definitionChildren defID libID projectID = readonly . defManagerOp libID projectID (\_ defManager -> do
    DefManager.gelem defID defManager `ifnot` ("Wrong 'defID' = " ++ show defID)
    let children = DefManager.children defManager defID
    return (defManager, children))


definitionParent :: Definition.ID 
                 -> Library.ID -> Project.ID -> Batch -> Either String (Definition.ID, Definition)
definitionParent defID libID projectID = readonly . defManagerOp libID projectID (\_ defManager -> do
    DefManager.gelem defID defManager `ifnot` ("Wrong 'defID' = " ++ show defID)
    parent <- DefManager.parent defManager defID <?> "Definition has no parent"
    return (defManager, parent))


resolveDefinition :: String -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String [(Definition.ID, Library.ID)]
resolveDefinition name parentID libID projectID = readonly . libManagerOp projectID (\_ libManager -> do
    result <- NameResolver.resolveDefinition name parentID libID libManager
    return (libManager, result))