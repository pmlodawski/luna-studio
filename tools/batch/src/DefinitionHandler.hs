---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module DefinitionHandler (
newDefinition,

addDefinition,
updateDefinition,
removeDefinition,

definitionChildren,
definitionParent
) 
where

import qualified Data.Vector as Vector
import           Data.Vector   (Vector)

import qualified Defs_Types as TDefs



newDefinition a type' flags attrs = do 
    putStrLn "NOT IMPLEMENTED - newDefinition"
    return $ TDefs.NodeDefinition type' flags attrs (Just 0) (Just 0)


addDefinition a (Just definition) parent = do
    putStrLn "NOT IMPLEMENTED - addDefinition"
    return $ definition


updateDefinition a definition = putStrLn "NOT IMPLEMENTED - updateDefinition"


removeDefinition a definition = putStrLn "NOT IMPLEMENTED - removeDefinition"

definitionChildren a definition = do 
    putStrLn "NOT IMPLEMENTED - definitionChildren"
    return $ Vector.fromList [] :: IO (Vector TDefs.NodeDefinition)


definitionParent a (Just definition) = do 
    putStrLn "NOT IMPLEMENTED - definitionParent"
    return definition
