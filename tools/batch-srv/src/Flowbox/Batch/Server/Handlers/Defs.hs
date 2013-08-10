---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Defs (
    defsGraph,

    newDefinition,

    addDefinition,
    updateDefinition,
    removeDefinition,

    definitionChildren,
    definitionParent,

    defOperation
) 
where

import           Data.IORef                                                  
import qualified Data.Vector                                               as Vector
import           Data.Vector                                                 (Vector)

import qualified Attrs_Types                                                 
import qualified Defs_Types                                                as TDefs
import           Flowbox.Batch.Server.Handlers.Common                        
import qualified Types_Types                                               as TTypes
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Batch                                         (Batch(..))
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Graph.Graph                          as Graph
import           Flowbox.Luna.Network.Graph.Graph                            (Graph)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()

 
-- TODO [PM] : refactor needed


------ public api helpers -----------------------------------------
defOperation :: (IORef Batch -> Definition.ID -> Definition -> a) -> IORef Batch 
             -> Maybe TDefs.Definition -> a
defOperation operation batchHandler tdefinition  = case tdefinition of 
    Nothing                       -> throw' "`definition` field is missing"
    Just tdef                     -> case (decode (tdef, Graph.empty) :: Either String (Int, Definition) ) of 
        Left message              -> throw' ("Failed to decode `definition` 2: " ++ message)
        Right (defID, definition) -> do operation batchHandler defID definition
            
    


defParentOperation :: (IORef Batch -> Definition -> Int -> a) -> IORef Batch
                   -> Maybe TDefs.Definition -> Maybe TDefs.Definition -> a
defParentOperation operation batchHandler mtdefinition mtparent = case mtdefinition of 
    Nothing                         -> throw' "`definition` field is missing"
    Just tdefinition                -> case decode (tdefinition, Graph.empty) :: Either String (Definition.ID, Definition) of
        Left message                -> throw' $ "Failed to decode `definition` 1: " ++ message
        Right (_, definition)       -> case mtparent of 
            Nothing                 -> throw' "`parent` field is missing"
            Just tparent            -> case decode (tparent, Graph.empty) :: Either String (Definition.ID, Definition) of 
                Left message        -> throw' $ "Failed to decode `parent`: " ++ message
                Right (parentID, _) -> operation batchHandler definition parentID
                

------ public api -------------------------------------------------


defsGraph :: IORef Batch -> IO TDefs.DefsGraph
defsGraph batchHandler = do
    putStrLn "call defsGraph"
    batch <- readIORef batchHandler
    case Batch.defsGraph batch of
        Left message -> throw' message
        Right adefManager -> return $ encode adefManager


newDefinition :: IORef Batch -> Maybe TTypes.Type -> Maybe (Vector TDefs.Import)
                            -> Maybe Attrs_Types.Flags -> Maybe Attrs_Types.Attributes
                            -> IO TDefs.Definition
newDefinition _ ttype timports tflags tattrs = do 
    putStrLn "Creating new definition...\t\tsuccess!"
    return $ TDefs.Definition ttype timports tflags tattrs (Just 0) (Just 0)


addDefinition :: IORef Batch -> Maybe TDefs.Definition
              -> Maybe TDefs.Definition -> IO TDefs.Definition
addDefinition = defParentOperation (\batchHandler definition parentID -> do
    putStrLn "call addDefinition"
    batch <- readIORef batchHandler
    case Batch.addDefinition definition parentID batch of
        Left message            -> throw' message
        Right (newBatch, defID) ->  do 
            let (newTDefinition, _) = encode (defID, definition)
            writeIORef batchHandler newBatch
            return $ newTDefinition)


updateDefinition :: IORef Batch -> Maybe TDefs.Definition -> IO ()
updateDefinition = defOperation (\batchHandler defID definition -> do
    putStrLn "call updateDefinition - NOT IMPLEMENTED, sorry."
    batch <- readIORef batchHandler
    case Batch.updateDefinition (defID, definition) batch of
        Left message   -> throw' message
        Right newBatch -> do 
            writeIORef batchHandler newBatch
            return ())


removeDefinition :: IORef Batch -> Maybe TDefs.Definition -> IO ()
removeDefinition = defOperation (\batchHandler defID _ -> do
    putStrLn "call removeDefinition"
    batch <- readIORef batchHandler
    case Batch.removeDefinition defID batch of
        Left message   -> throw' message
        Right newBatch -> do 
            writeIORef batchHandler newBatch
            return ())


definitionChildren :: IORef Batch -> Maybe TDefs.Definition -> IO (Vector TDefs.Definition)
definitionChildren = defOperation (\batchHandler defID _ -> do
    putStrLn "call definitionChildren"  
    batch <- readIORef batchHandler
    case Batch.definitionChildren defID batch of
        Left message   -> throw' message
        Right children -> do 
           let tchildrenWithGraph = map (encode) children
               tchildren = map (\(def, _) -> def) tchildrenWithGraph
           return $ Vector.fromList tchildren)


definitionParent :: IORef Batch -> Maybe TDefs.Definition -> IO TDefs.Definition
definitionParent = defOperation (\batchHandler defID _ -> do
    putStrLn "call definitionParent"
    batch <- readIORef batchHandler
    case Batch.definitionParent defID batch of
        Left message -> throw' message
        Right parent -> do 
            let (tparent, _) = encode parent :: (TDefs.Definition, Graph)
            return tparent)

