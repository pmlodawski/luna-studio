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
import           Batch_Types (ArgumentException(..))
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
                            -> IO Defs_Types.NodeDef
newDefinition _ ttype timports tflags tattrs = do 
    putStrLn "Creating new definition...\t\tsuccess!"
    return $ Defs_Types.NodeDef ttype timports tflags tattrs (Just 0) (Just 0)


addDefinition :: IORef Core -> Maybe Defs_Types.NodeDef
              -> Maybe Defs_Types.NodeDef -> IO Defs_Types.NodeDef
addDefinition = defParentOperation (\batchHandler definition parentID -> do
    putStrLn "call libraryRootDef"
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    case DefManager.gelem parentID defManager of 
        True -> do
                let [defID]    = DefManager.newNodes 1 defManager
                    newCore    = core { Core.defManager = DefManager.addToParent (parentID, defID, definition) defManager }
                    (newTDefinition, _) = encode (defID, definition)
                writeIORef batchHandler newCore
                return $ newTDefinition
        False -> throw $ ArgumentException $ Just $ Text.pack "Wrong `defID` in `parent` field")


updateDefinition :: IORef Core -> Maybe Defs_Types.NodeDef -> IO ()
updateDefinition = defOperation (\batchHandler definition -> do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    putStrLn "NOT IMPLEMENTED - updateDefinition")


removeDefinition :: IORef Core -> Maybe Defs_Types.NodeDef -> IO ()
removeDefinition = defOperation (\batchHandler definition -> do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    putStrLn "NOT IMPLEMENTED - removeDefinition")


definitionChildren :: IORef Core -> Maybe Defs_Types.NodeDef -> IO (Vector Defs_Types.NodeDef)
definitionChildren = defOperation (\batchHandler definition -> do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    putStrLn "NOT IMPLEMENTED - definitionChildren"  
    return $ Vector.fromList [] :: IO (Vector Defs_Types.NodeDef))


definitionParent :: IORef Core -> Maybe Defs_Types.NodeDef -> IO Defs_Types.NodeDef
definitionParent = defOperation (\batchHandler definition -> do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    putStrLn "NOT IMPLEMENTED - definitionParent"
    return undefined)


defOperation :: (IORef Core -> NodeDef -> a) -> IORef Core 
             -> Maybe Defs_Types.NodeDef -> a
defOperation operation batchHandler tdefinition  = case tdefinition of 
    (Just tdef) -> do
        case (decode (tdef, Graph.empty) :: Either String (Int, NodeDef) ) of 
            Right (_, definition) -> do operation batchHandler definition
            Left message          -> throw $ ArgumentException $ Just $ Text.pack ("Failed to decode `definition` 2: " ++ message)
    Nothing            -> throw $ ArgumentException $ Just $ Text.pack "`definition` field is missing"


defParentOperation :: (IORef Core -> NodeDef -> Int -> a) -> IORef Core
                   -> Maybe Defs_Types.NodeDef -> Maybe Defs_Types.NodeDef -> a
defParentOperation operation batchHandler tdefinition tparent = case (tdefinition, tparent) of 
    (Just tdef, Just tpar) -> do
        case (decode (tdef,  Graph.empty) :: Either String (Int, NodeDef) ,
              decode (tpar,  Graph.empty) :: Either String (Int, NodeDef) ) of 
            (Right (_, definition), Right (parentID, _))
                              -> do operation batchHandler definition parentID
            (_, Left message) -> throw $ ArgumentException $ Just $ Text.pack ("Failed to decode `parent`: " ++ message)
            (Left message, _) -> throw $ ArgumentException $ Just $ Text.pack ("Failed to decode `definition` 1: " ++ message)
    ( _       , Nothing )            -> throw $ ArgumentException $ Just $ Text.pack "`parent` field is missing"
    ( _       , _       )            -> throw $ ArgumentException $ Just $ Text.pack "`definition` field is missing"