---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Defs (
    defsGraph,
    defByID, 

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
import           Flowbox.Batch.Server.Handlers.Common                  (logger, tRunScript)
import           Flowbox.Batch.Batch                                   (Batch(..))
import qualified Flowbox.Batch.Handlers.Defs                         as BatchD
import           Flowbox.Control.Error                                 
import qualified Flowbox.Luna.Network.Def.Definition                 as Definition
import           Flowbox.Luna.Network.Def.Definition                   (Definition)
import qualified Flowbox.Luna.Network.Graph.Graph                    as Graph
import           Flowbox.Luna.Network.Graph.Graph                      (Graph)
import qualified Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs as CDefs
import           Flowbox.System.Log.Logger                             
import           Flowbox.Tools.Conversion                              



------ public api -------------------------------------------------

defsGraph :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO TDefs.DefsGraph
defsGraph batchHandler mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called defsGraph"
    libID       <- tryGetID mtlibID "libID"
    projectID   <- tryGetID mtprojectID "projectID"
    batch       <- tryReadIORef batchHandler
    adefManager <- tryRight $ BatchD.defsGraph libID projectID batch

    return $ CDefs.toDefsGraph adefManager


defByID :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO TDefs.Definition
defByID batchHandler mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called defByID"
    defID      <- tryGetID mtdefID     "defID"    
    libID      <- tryGetID mtlibID     "libID"
    projectID  <- tryGetID mtprojectID "projectID"
    batch      <- tryReadIORef batchHandler
    definition <- tryRight $ BatchD.defByID defID libID projectID batch
    return $ fst $ encode (defID, definition)

addDefinition :: IORef Batch 
              -> Maybe TDefs.Definition -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO TDefs.Definition
addDefinition batchHandler mtdefinition mtparentID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called addDefinition"
    tdefinition       <- mtdefinition <??> "'definition' argument is missing"
    (_, definition)   <- tryRight (decode (tdefinition, Graph.empty) :: Either String (Definition.ID, Definition))
    parentID          <- tryGetID mtparentID "parentID"    
    libID             <- tryGetID mtlibID    "libID"
    projectID         <- tryGetID mtprojectID "projectID"
    batch             <- tryReadIORef batchHandler
    (newBatch, defID) <- tryRight $ BatchD.addDefinition definition parentID libID projectID batch
    tryWriteIORef batchHandler newBatch
    return $ fst $ encode (defID, definition)


updateDefinition :: IORef Batch -> Maybe TDefs.Definition -> Maybe Int32 -> Maybe Int32 -> IO ()
updateDefinition batchHandler mtdefinition mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called updateDefinition"
    tdefinition <- mtdefinition <??> "'definition' field is missing" 
    definition  <- tryRight $ decode (tdefinition, Graph.empty) -- :: (Definition.ID, Definition)
    libID       <- tryGetID mtlibID "libID"
    projectID   <- tryGetID mtprojectID "projectID"
    batch       <- tryReadIORef batchHandler
    newBatch    <- tryRight $ BatchD.updateDefinition definition libID projectID batch
    tryWriteIORef batchHandler newBatch


removeDefinition :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO ()
removeDefinition batchHandler mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called removeDefinition"
    defID       <- tryGetID mtdefID "defID"
    libID       <- tryGetID mtlibID "libID"
    projectID   <- tryGetID mtprojectID "projectID"
    batch       <- tryReadIORef batchHandler
    newBatch    <- tryRight $ BatchD.removeDefinition defID libID projectID batch 
    tryWriteIORef batchHandler newBatch
    return ()
 

definitionChildren :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO (Vector TDefs.Definition)
definitionChildren batchHandler mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called definitionChildren"
    defID       <- tryGetID mtdefID "defID"
    libID       <- tryGetID mtlibID "libID"
    projectID   <- tryGetID mtprojectID "projectID"
    batch       <- tryReadIORef batchHandler
    children    <- tryRight $ BatchD.definitionChildren defID libID projectID batch
    let tchildrenWithGraph = map (encode) children
        tchildren = map (\(def, _) -> def) tchildrenWithGraph
    return $ Vector.fromList tchildren


definitionParent :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO TDefs.Definition
definitionParent batchHandler mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called definitionParent"
    defID       <- tryGetID mtdefID "defID"
    libID       <- tryGetID mtlibID "libID"
    projectID   <- tryGetID mtprojectID "projectID"
    batch       <- tryReadIORef batchHandler
    parent      <- tryRight $ BatchD.definitionParent defID libID projectID batch
    return $ fst (encode parent :: (TDefs.Definition, Graph))

