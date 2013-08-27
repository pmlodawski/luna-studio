---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Defs (
    defsGraph,

    addDefinition,
    updateDefinition,
    removeDefinition,

    definitionChildren,
    definitionParent,
) 
where


import           Data.Int                                              
import           Data.IORef                                            
import qualified Data.Vector                                         as Vector
import           Data.Vector                                           (Vector)

import qualified Defs_Types                                          as TDefs

import           Flowbox.Batch.Server.Handlers.Common                  
import qualified Flowbox.Batch.Batch                                 as Batch
import           Flowbox.Batch.Batch                                   (Batch(..))
import           Flowbox.Control.Error                                 
import qualified Flowbox.Luna.Network.Def.Definition                 as Definition
import           Flowbox.Luna.Network.Def.Definition                   (Definition)
import qualified Flowbox.Luna.Network.Graph.Graph                    as Graph
import           Flowbox.Luna.Network.Graph.Graph                      (Graph)
import qualified Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs as CDefs
import           Flowbox.Tools.Conversion                              


------ public api -------------------------------------------------

defsGraph :: IORef Batch -> Maybe Int32 -> IO TDefs.DefsGraph
defsGraph batchHandler mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called defsGraph"
    libID       <- tryGetID mtlibID "libID"
    batch       <- tryReadIORef batchHandler
    adefManager <- tryRight $ Batch.defsGraph libID batch

    return $ CDefs.toDefsGraph adefManager


addDefinition :: IORef Batch 
              -> Maybe TDefs.Definition -> Maybe Int32 -> Maybe Int32 -> IO TDefs.Definition
addDefinition batchHandler mtdefinition mtparentID mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called addDefinition"
    tdefinition       <- mtdefinition <??> "'definition' argument is missing"
    (_, definition)   <- tryRight (decode (tdefinition, Graph.empty) :: Either String (Definition.ID, Definition))
    parentID          <- tryGetID mtparentID "parentID"    
    libID             <- tryGetID mtlibID    "libID"
    batch             <- tryReadIORef batchHandler
    (newBatch, defID) <- tryRight $ Batch.addDefinition definition parentID libID batch
    tryWriteIORef batchHandler newBatch
    return $ fst $ encode (defID, definition)


updateDefinition :: IORef Batch -> Maybe TDefs.Definition -> Maybe Int32 -> IO ()
updateDefinition batchHandler mtdefinition mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called updateDefinition"
    
    tdefinition <- mtdefinition <??> "'definition' field is missing" 
    definition  <- tryRight $ decode (tdefinition, Graph.empty) -- :: (Definition.ID, Definition)
    libID       <- tryGetID mtlibID "libID"
    
    batch       <- tryReadIORef batchHandler
    newBatch    <- tryRight $ Batch.updateDefinition definition libID batch
    tryWriteIORef batchHandler newBatch


removeDefinition :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO ()
removeDefinition batchHandler mtdefID mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called removeDefinition"
    defID       <- tryGetID mtdefID "defID"
    libID       <- tryGetID mtlibID "libID"
    batch       <- tryReadIORef batchHandler
    newBatch    <- tryRight $ Batch.removeDefinition defID libID batch 
    tryWriteIORef batchHandler newBatch
    return ()
 

definitionChildren :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO (Vector TDefs.Definition)
definitionChildren batchHandler mtdefID mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called definitionChildren"
    defID       <- tryGetID mtdefID "defID"
    libID       <- tryGetID mtlibID "libID"
    batch       <- tryReadIORef batchHandler
    children    <- tryRight $ Batch.definitionChildren defID libID batch
    let tchildrenWithGraph = map (encode) children
        tchildren = map (\(def, _) -> def) tchildrenWithGraph
    return $ Vector.fromList tchildren


definitionParent :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO TDefs.Definition
definitionParent batchHandler mtdefID mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called definitionParent"
    defID       <- tryGetID mtdefID "defID"
    libID       <- tryGetID mtlibID "libID"
    batch       <- tryReadIORef batchHandler
    parent      <- tryRight $ Batch.definitionParent defID libID batch
    return $ fst (encode parent :: (TDefs.Definition, Graph))

