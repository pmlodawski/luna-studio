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
import qualified Flowbox.Luna.Lib.Library            as Library
import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition(..))



defsGraph :: Library.ID -> Batch -> Either String DefManager
defsGraph libID = readonly . defManagerOp libID (\_ defManager -> 
    Right (defManager, defManager))


addDefinition :: Definition -> Definition.ID -> Library.ID -> Batch 
              -> Either String (Batch, Definition.ID)
addDefinition definition parentID libID = defManagerOp libID (\_ defManager ->
    case DefManager.gelem parentID defManager of 
        False -> Left "Wrong `defID`"
        True  -> Right $ DefManager.addNewToParent (parentID, definition) defManager)


updateDefinition :: (Definition.ID, Definition) -> Library.ID -> Batch -> Either String Batch
updateDefinition (defID, def) libID = noresult . definitionOp defID libID (\_ _ ->
    Right (def, def))


removeDefinition :: Definition.ID -> Library.ID -> Batch -> Either String Batch 
removeDefinition defID libID = noresult . defManagerOp libID (\_ defManager -> 
    case DefManager.gelem defID defManager of 
        False -> Left "Wrong `defID`"
        True  -> Right (newDefManager, ()) where 
                        newDefManager = DefManager.delete defID defManager)


definitionChildren :: Definition.ID -> Library.ID -> Batch
                   -> Either String [(Definition.ID, Definition)]
definitionChildren defID libID = readonly . defManagerOp libID (\_ defManager -> 
    case DefManager.gelem defID defManager of 
        False -> Left "Wrong `defID`"
        True  -> Right (defManager, children) where
                       children = DefManager.children defManager defID)


definitionParent :: Definition.ID -> Library.ID -> Batch -> Either String (Definition.ID, Definition)
definitionParent defID libID = readonly . defManagerOp libID (\_ defManager -> 
    case DefManager.gelem defID defManager of 
        False           -> Left "Wrong `defID`"
        True            -> case DefManager.parent defManager defID of
            Nothing     -> Left "Definition has no parent"
            Just parent -> Right (defManager, parent))
