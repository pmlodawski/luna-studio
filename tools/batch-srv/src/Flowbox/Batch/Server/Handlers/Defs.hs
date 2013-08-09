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
import qualified Data.Vector                                      as Vector
import           Data.Vector                                        (Vector)

import qualified Attrs_Types
import qualified Defs_Types                                                as TDefs
import           Flowbox.Batch.Server.Handlers.Common
import qualified Types_Types                                               as TTypes
import qualified Flowbox.Batch.Project.Project                             as Project
import           Flowbox.Batch.Project.Project                               (Project)
import qualified Flowbox.Luna.Core                                         as Core
import           Flowbox.Luna.Core                                           (Core(..))
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Graph.Graph                          as Graph
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()


-- TODO [PM] : refactor needed


------ public api helpers -----------------------------------------
defOperation :: (IORef Project -> Definition.ID -> Definition -> a) -> IORef Project 
             -> Maybe TDefs.Definition -> a
defOperation operation batchHandler tdefinition  = case tdefinition of 
    Nothing                       -> throw' "`definition` field is missing"
    Just tdef                     -> case (decode (tdef, Graph.empty) :: Either String (Int, Definition) ) of 
        Left message              -> throw' ("Failed to decode `definition` 2: " ++ message)
        Right (defID, definition) -> do operation batchHandler defID definition
            
    


defParentOperation :: (IORef Project -> Definition -> Int -> a) -> IORef Project
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


defsGraph :: IORef Project -> IO TDefs.DefsGraph
defsGraph batchHandler = do
    putStrLn "call defsGraph"
    project <- readIORef batchHandler
    let core       = Project.core project
        defManager = Core.defManager core
        tdefManager = encode defManager
    return tdefManager


newDefinition :: IORef Project -> Maybe TTypes.Type -> Maybe (Vector TDefs.Import)
                            -> Maybe Attrs_Types.Flags -> Maybe Attrs_Types.Attributes
                            -> IO TDefs.Definition
newDefinition _ ttype timports tflags tattrs = do 
    putStrLn "Creating new definition...\t\tsuccess!"
    return $ TDefs.Definition ttype timports tflags tattrs (Just 0) (Just 0)


addDefinition :: IORef Project -> Maybe TDefs.Definition
              -> Maybe TDefs.Definition -> IO TDefs.Definition
addDefinition = defParentOperation (\batchHandler definition parentID -> do
    putStrLn "call addDefinition"
    project <- readIORef batchHandler
    let core       = Project.core project
        defManager = Core.defManager core
    case DefManager.gelem parentID defManager of 
        False -> throw' "Wrong `defID` in `parent` field"
        True  -> do let [defID]       = DefManager.newNodes 1 defManager
                        newDefManager = DefManager.addToParent (parentID, defID, definition) defManager
                        newCore       = core    { Core.defManager = newDefManager }
                        newProject    = project { Project.core = newCore }
                        (newTDefinition, _) = encode (defID, definition)
                    writeIORef batchHandler newProject
                    return $ newTDefinition)


updateDefinition :: IORef Project -> Maybe TDefs.Definition -> IO ()
updateDefinition = defOperation (\batchHandler defID definition -> do
    putStrLn "call updateDefinition - NOT IMPLEMENTED, sorry."
    project <- readIORef batchHandler
    let core       = Project.core project
        defManager = Core.defManager core
    return ())


removeDefinition :: IORef Project -> Maybe TDefs.Definition -> IO ()
removeDefinition = defOperation (\batchHandler defID _ -> do
    putStrLn "call removeDefinition"
    project <- readIORef batchHandler
    let core       = Project.core project
        defManager = Core.defManager core
    case DefManager.gelem defID defManager of 
        False -> throw' "Wrong `defID` in `definition` field"
        True -> do let newDefManager = DefManager.delNode defID defManager
                       newCore       = core    { Core.defManager = newDefManager }
                       newProject    = project { Project.core = newCore }
                   writeIORef batchHandler newProject)


definitionChildren :: IORef Project -> Maybe TDefs.Definition -> IO (Vector TDefs.Definition)
definitionChildren = defOperation (\batchHandler defID _ -> do
    putStrLn "call definitionChildren"  
    project <- readIORef batchHandler
    let core       = Project.core project
        defManager = Core.defManager core
    case DefManager.gelem defID defManager of 
        False -> throw' "Wrong `defID` in `definition` field"
        True -> do let children = DefManager.children defManager defID
                       tchildrenWithGraph = map (encode) children
                       tchildren = map (\(def, _) -> def) tchildrenWithGraph
                   return $ Vector.fromList tchildren)


definitionParent :: IORef Project -> Maybe TDefs.Definition -> IO TDefs.Definition
definitionParent = defOperation (\batchHandler defID _ -> do
    putStrLn "call definitionParent"
    project <- readIORef batchHandler
    let core       = Project.core project
        defManager = Core.defManager core
    case DefManager.gelem defID defManager of 
        False -> throw' "Wrong `defID` in `definition` field"
        True -> do let parent = DefManager.parent defManager defID
                   case parent of 
                       Nothing -> -- TODO [PM] : what if there is no parent?
                                  undefined
                       Just p  -> do let (tparent, _) = encode p
                                     return tparent)

