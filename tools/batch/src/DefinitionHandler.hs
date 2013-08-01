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

import           Control.Exception
import           Data.IORef
import qualified Data.Text.Lazy as Text
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)

import qualified Attrs_Types
import           Batch_Types (MissingFieldsException(..))
import qualified Defs_Types
import qualified Types_Types
import qualified Luna.Core                     as Core
import qualified Luna.Network.Graph.Graph      as Graph
import           Luna.Network.Def.NodeDef        (NodeDef)
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Defs    ()

newDefinition :: batchHandler -> Maybe Types_Types.Type -> Maybe (Vector Defs_Types.Import)
                              -> Maybe Attrs_Types.Flags -> Maybe Attrs_Types.Attributes
                              -> IO Defs_Types.NodeDefinition
newDefinition _ ttype timports tflags tattrs = do 
    putStrLn "Creating new definition...\t\tsuccess!"
    return $ Defs_Types.NodeDefinition ttype timports tflags tattrs (Just 0) (Just 0)


addDefinition batchHandler (Just tdefinition) (Just tparent) = do
    core <- readIORef batchHandler
    case (decode (tdefinition, Graph.empty) :: Either String (Int, NodeDef) ) of 
        Right (_, definition) -> do putStrLn "NOT IMPLEMENTED - addDefinition"
                                    return $ tdefinition
        Left message          -> throw $ MissingFieldsException $ Just $ Text.pack "Some fields are missing"

addDefinition batchHandler _ _ = do
    throw $ MissingFieldsException $ Just $ Text.pack "Some fields are missing"


updateDefinition batchHandler args = case args of
    (Just tdefinition) -> putStrLn "NOT IMPLEMENTED - updateDefinition"

updateDefinition batchHandler Nothing = do 
    throw $ MissingFieldsException $ Just $ Text.pack "`definition` field is missing"


removeDefinition batchHandler (Just tdefinition) = putStrLn "NOT IMPLEMENTED - removeDefinition"

removeDefinition batchHandler Nothing = do 
    throw $ MissingFieldsException $ Just $ Text.pack "`definition` field is missing"


definitionChildren batchHandler (Just tdefinition) = do 
    putStrLn "NOT IMPLEMENTED - definitionChildren"
    return $ Vector.fromList [] :: IO (Vector Defs_Types.NodeDefinition)

definitionChildren batchHandler Nothing = do 
    throw $ MissingFieldsException $ Just $ Text.pack "`definition` field is missing"


definitionParent batchHandler (Just tdefinition) = do 
    putStrLn "NOT IMPLEMENTED - definitionParent"
    return tdefinition

definitionParent batchHandler Nothing = do 
    throw $ MissingFieldsException $ Just $ Text.pack "`definition` field is missing"
