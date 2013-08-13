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

import qualified Attrs_Types                                               as TAttrs
import qualified Defs_Types                                                as TDefs
import qualified Libs_Types                                                as TLibs
import qualified Types_Types                                               as TTypes

import           Flowbox.Batch.Server.Handlers.Common                        
import           Flowbox.Batch.Server.Handlers.Libs                          (libOperation)
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Batch                                         (Batch(..))
import qualified Flowbox.Luna.Lib.Library                                  as Library
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Graph.Graph                          as Graph
import           Flowbox.Luna.Network.Graph.Graph                            (Graph)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()

 
------ public api helpers -----------------------------------------

defOperation :: ((Definition.ID, Definition) -> Library.ID -> result)
             -> Maybe TDefs.Definition -> Maybe TLibs.Library -> result
defOperation operation mtdefinition = libOperation (\ (libID, _) -> 
    case mtdefinition of 
        Nothing                       -> throw' "`definition` field is missing"
        Just tdefinition              -> case (decode (tdefinition, Graph.empty) :: Either String (Int, Definition) ) of 
            Left message              -> throw' ("Failed to decode `definition` 2: " ++ message)
            Right (defID, definition) -> operation (defID, definition) libID)


defParentOperation :: (Definition -> Int -> Library.ID -> result)
                   -> Maybe TDefs.Definition -> Maybe TDefs.Definition
                   -> Maybe TLibs.Library -> result
defParentOperation operation mtdefinition mtparent = libOperation (\ (libID, _) -> 
    case mtdefinition of 
        Nothing                         -> throw' "`definition` field is missing"
        Just tdefinition                -> case decode (tdefinition, Graph.empty) :: Either String (Definition.ID, Definition) of
            Left message                -> throw' $ "Failed to decode `definition` 1: " ++ message
            Right (_, definition)       -> case mtparent of 
                Nothing                 -> throw' "`parent` field is missing"
                Just tparent            -> case decode (tparent, Graph.empty) :: Either String (Definition.ID, Definition) of 
                    Left message        -> throw' $ "Failed to decode `parent`: " ++ message
                    Right (parentID, _) -> operation definition parentID libID)


------ public api -------------------------------------------------

defsGraph :: IORef Batch -> Maybe TLibs.Library -> IO TDefs.DefsGraph
defsGraph batchHandler = libOperation (\ (libID, _) -> do
    putStrLn "call defsGraph"
    batch <- readIORef batchHandler
    case Batch.defsGraph libID batch of
        Left message -> throw' message
        Right adefManager -> return $ encode adefManager)


newDefinition :: IORef Batch -> Maybe TTypes.Type -> Maybe (Vector TDefs.Import)
                            -> Maybe TAttrs.Flags -> Maybe TAttrs.Attributes
                            -> IO TDefs.Definition
newDefinition _ ttype timports tflags tattrs = do 
    putStrLn "Creating new definition...\t\tsuccess!"
    return $ TDefs.Definition ttype timports tflags tattrs (Just 0)


addDefinition :: IORef Batch 
              -> Maybe TDefs.Definition -> Maybe TDefs.Definition 
              -> Maybe TLibs.Library -> IO TDefs.Definition
addDefinition batchHandler = defParentOperation (\definition parentID libID -> do
    putStrLn "call addDefinition"
    batch <- readIORef batchHandler
    case Batch.addDefinition definition parentID libID batch of
        Left message            -> throw' message
        Right (newBatch, defID) ->  do 
            let (newTDefinition, _) = encode (defID, definition)
            writeIORef batchHandler newBatch
            return $ newTDefinition)


updateDefinition :: IORef Batch -> Maybe TDefs.Definition -> Maybe TLibs.Library -> IO ()
updateDefinition batchHandler = defOperation (\(defID, definition) libID -> do
    putStrLn "call updateDefinition"
    batch <- readIORef batchHandler
    case Batch.updateDefinition (defID, definition) libID batch of
        Left message   -> throw' message
        Right newBatch -> do 
            writeIORef batchHandler newBatch
            return ())


removeDefinition :: IORef Batch -> Maybe TDefs.Definition -> Maybe TLibs.Library -> IO ()
removeDefinition batchHandler = defOperation (\(defID, _) libID -> do
    putStrLn "call removeDefinition"
    batch <- readIORef batchHandler
    case Batch.removeDefinition defID libID batch of
        Left message   -> throw' message
        Right newBatch -> do 
            writeIORef batchHandler newBatch
            return ())


definitionChildren :: IORef Batch -> Maybe TDefs.Definition 
                   -> Maybe TLibs.Library -> IO (Vector TDefs.Definition)
definitionChildren batchHandler = defOperation (\(defID, _) libID -> do
    putStrLn "call definitionChildren"  
    batch <- readIORef batchHandler
    case Batch.definitionChildren defID libID batch of
        Left message   -> throw' message
        Right children -> do 
           let tchildrenWithGraph = map (encode) children
               tchildren = map (\(def, _) -> def) tchildrenWithGraph
           return $ Vector.fromList tchildren)


definitionParent :: IORef Batch -> Maybe TDefs.Definition 
                 -> Maybe TLibs.Library -> IO TDefs.Definition
definitionParent batchHandler = defOperation (\(defID, _) libID -> do
    putStrLn "call definitionParent"
    batch <- readIORef batchHandler
    case Batch.definitionParent defID libID batch of
        Left message -> throw' message
        Right parent -> do 
            let (tparent, _) = encode parent :: (TDefs.Definition, Graph)
            return tparent)

