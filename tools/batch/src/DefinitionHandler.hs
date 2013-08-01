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
import           Luna.Core                       (Core)
import qualified Luna.Network.Def.DefManager   as DefManager
import           Luna.Network.Def.NodeDef        (NodeDef)
import qualified Luna.Network.Graph.Graph      as Graph
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Defs    ()

newDefinition :: IORef Core -> Maybe Types_Types.Type -> Maybe (Vector Defs_Types.Import)
                            -> Maybe Attrs_Types.Flags -> Maybe Attrs_Types.Attributes
                            -> IO Defs_Types.NodeDefinition
newDefinition _ ttype timports tflags tattrs = do 
    putStrLn "Creating new definition...\t\tsuccess!"
    return $ Defs_Types.NodeDefinition ttype timports tflags tattrs (Just 0) (Just 0)


addDefinition :: IORef Core -> Maybe Defs_Types.NodeDefinition
              -> Maybe Defs_Types.NodeDefinition -> IO Defs_Types.NodeDefinition
addDefinition = defParentOperation (\batchHandler definition parent -> do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
        [dID]      = DefManager.newNodes 1 defManager
        newCore    = core { Core.defManager = DefManager.insNode (dID, definition) defManager }
        (newTDefinition, _) = encode (dID, definition)
    writeIORef batchHandler core
    putStrLn "NOT IMPLEMENTED - addDefinition"
    return $ newTDefinition)


updateDefinition :: IORef Core -> Maybe Defs_Types.NodeDefinition -> IO ()
updateDefinition = defOperation (\batchHandler definition -> do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    putStrLn "NOT IMPLEMENTED - updateDefinition")


removeDefinition :: IORef Core -> Maybe Defs_Types.NodeDefinition -> IO ()
removeDefinition = defOperation (\batchHandler definition -> do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    putStrLn "NOT IMPLEMENTED - removeDefinition")


definitionChildren :: IORef Core -> Maybe Defs_Types.NodeDefinition -> IO (Vector Defs_Types.NodeDefinition)
definitionChildren = defOperation (\batchHandler definition -> do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    putStrLn "NOT IMPLEMENTED - definitionChildren"  
    return $ Vector.fromList [] :: IO (Vector Defs_Types.NodeDefinition))


definitionParent :: IORef Core -> Maybe Defs_Types.NodeDefinition -> IO Defs_Types.NodeDefinition
definitionParent = defOperation (\batchHandler definition -> do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    putStrLn "NOT IMPLEMENTED - definitionParent"
    return undefined)


defOperation :: (IORef Core -> NodeDef -> a) -> IORef Core 
             -> Maybe Defs_Types.NodeDefinition -> a
defOperation fun batchHandler tdefinition  = case tdefinition of 
    (Just tdef) -> do
        case (decode (tdef, Graph.empty) :: Either String (Int, NodeDef) ) of 
            Right (_, definition) -> do fun batchHandler definition
            Left message          -> throw $ MissingFieldsException $ Just $ Text.pack ("Failed to decode `definition`: " ++ message)
    Nothing            -> throw $ MissingFieldsException $ Just $ Text.pack "`definition` field is missing"


defParentOperation :: (IORef Core -> NodeDef -> NodeDef -> a) -> IORef Core
                   -> Maybe Defs_Types.NodeDefinition -> Maybe Defs_Types.NodeDefinition -> a
defParentOperation fun batchHandler tdefinition tparent = case (tdefinition, tparent) of 
    (Just tdef, Just tpar) -> do
        case (decode (tdef,  Graph.empty) :: Either String (Int, NodeDef) ,
              decode (tpar,      Graph.empty) :: Either String (Int, NodeDef) ) of 
            (Right (_, definition), Right (_, parent))
                              -> do fun batchHandler definition parent
            (_, Left message) -> throw $ MissingFieldsException $ Just $ Text.pack ("Failed to decode `definition`: " ++ message)
            (Left message, _) -> throw $ MissingFieldsException $ Just $ Text.pack ("Failed to decode `definition`: " ++ message)
    (_, _)            -> throw $ MissingFieldsException $ Just $ Text.pack "`Some fields are missing"
