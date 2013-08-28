---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Defs (
    defsGraph,
    addDefinition,
    updateDefinition ,
    removeDefinition,
    definitionChildren,
    definitionParent,

    defManagerOp,
    definitionOp,
) where

import           Flowbox.Batch.Batch                   (Batch(..))
import           Flowbox.Batch.Handlers.Common         (noresult, readonly, defManagerOp, definitionOp)
import           Flowbox.Control.Error                 ((<?>), ifnot)
import qualified Flowbox.Luna.Lib.Library            as Library
import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition(..))



defsGraph :: Library.ID -> Batch -> Either String DefManager
defsGraph libID = readonly . defManagerOp libID (\_ defManager -> do
    return (defManager, defManager))


addDefinition :: Definition -> Definition.ID -> Library.ID -> Batch 
              -> Either String (Batch, Definition.ID)
addDefinition definition parentID libID = defManagerOp libID (\_ defManager -> do
    DefManager.gelem parentID defManager `ifnot` "Wrong `defID`"
    return $ DefManager.addNewToParent (parentID, definition) defManager)


updateDefinition :: (Definition.ID, Definition) -> Library.ID -> Batch -> Either String Batch
updateDefinition (defID, def) libID = noresult . definitionOp defID libID (\_ _ -> do
    return (def, def))


removeDefinition :: Definition.ID -> Library.ID -> Batch -> Either String Batch 
removeDefinition defID libID = noresult . defManagerOp libID (\_ defManager -> do
    DefManager.gelem defID defManager `ifnot` "Wrong `defID`"
    let newDefManager = DefManager.delete defID defManager
    return (newDefManager, ()))


definitionChildren :: Definition.ID -> Library.ID -> Batch
                   -> Either String [(Definition.ID, Definition)]
definitionChildren defID libID = readonly . defManagerOp libID (\_ defManager -> do
    DefManager.gelem defID defManager `ifnot` "Wrong `defID`"
    let children = DefManager.children defManager defID
    return (defManager, children))


definitionParent :: Definition.ID -> Library.ID -> Batch -> Either String (Definition.ID, Definition)
definitionParent defID libID = readonly . defManagerOp libID (\_ defManager -> do
    DefManager.gelem defID defManager `ifnot` "Wrong `defID`"
    parent <- DefManager.parent defManager defID <?> "Definition has no parent"
    return (defManager, parent))
