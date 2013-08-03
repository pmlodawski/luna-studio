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
definitionParent,

defOperation
) 
where

import           Control.Exception
import           Data.IORef
import           Data.Text.Lazy   (pack, unpack)
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)

import qualified Attrs_Types
import           Batch_Types (ArgumentException(..))
import qualified Defs_Types                    as TDefs
import qualified Types_Types                   as TTypes
import qualified Luna.Core                     as Core
import           Luna.Core                       (Core)
import qualified Luna.Network.Def.DefManager   as DefManager
import qualified Luna.Network.Def.NodeDef      as NodeDef
import           Luna.Network.Def.NodeDef        (NodeDef)
import qualified Luna.Network.Graph.Graph      as Graph
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Defs    ()


-- TODO [PM] : refactor needed


------ public api helpers -----------------------------------------
defOperation :: (IORef Core -> NodeDef.ID -> NodeDef -> a) -> IORef Core 
             -> Maybe TDefs.NodeDef -> a
defOperation operation batchHandler tdefinition  = case tdefinition of 
    (Just tdef) -> do
        case (decode (tdef, Graph.empty) :: Either String (Int, NodeDef) ) of 
            Right (defID, definition) -> do operation batchHandler defID definition
            Left message          -> throw $ ArgumentException $ Just $ pack ("Failed to decode `definition` 2: " ++ message)
    Nothing            -> throw $ ArgumentException $ Just $ pack "`definition` field is missing"


defParentOperation :: (IORef Core -> NodeDef -> Int -> a) -> IORef Core
                   -> Maybe TDefs.NodeDef -> Maybe TDefs.NodeDef -> a
defParentOperation operation batchHandler tdefinition tparent = case (tdefinition, tparent) of 
    (Just tdef, Just tpar) -> do
        case (decode (tdef,  Graph.empty) :: Either String (Int, NodeDef) ,
              decode (tpar,  Graph.empty) :: Either String (Int, NodeDef) ) of 
            (Right (_, definition), Right (parentID, _))
                              -> do operation batchHandler definition parentID
            (_, Left message) -> throw $ ArgumentException $ Just $ pack ("Failed to decode `parent`: " ++ message)
            (Left message, _) -> throw $ ArgumentException $ Just $ pack ("Failed to decode `definition` 1: " ++ message)
    ( _       , Nothing )            -> throw $ ArgumentException $ Just $ pack "`parent` field is missing"
    ( _       , _       )            -> throw $ ArgumentException $ Just $ pack "`definition` field is missing"


------ public api -------------------------------------------------
newDefinition :: IORef Core -> Maybe TTypes.Type -> Maybe (Vector TDefs.Import)
                            -> Maybe Attrs_Types.Flags -> Maybe Attrs_Types.Attributes
                            -> IO TDefs.NodeDef
newDefinition _ ttype timports tflags tattrs = do 
    putStrLn "Creating new definition...\t\tsuccess!"
    return $ TDefs.NodeDef ttype timports tflags tattrs (Just 0) (Just 0)


addDefinition :: IORef Core -> Maybe TDefs.NodeDef
              -> Maybe TDefs.NodeDef -> IO TDefs.NodeDef
addDefinition = defParentOperation (\batchHandler definition parentID -> do
    putStrLn "call addDefinition"
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    case DefManager.gelem parentID defManager of 
        True -> do
                let [defID]    = DefManager.newNodes 1 defManager
                    newCore    = core { Core.defManager = DefManager.addToParent (parentID, defID, definition) defManager }
                    (newTDefinition, _) = encode (defID, definition)
                writeIORef batchHandler newCore
                return $ newTDefinition
        False -> throw $ ArgumentException $ Just $ pack "Wrong `defID` in `parent` field")


updateDefinition :: IORef Core -> Maybe TDefs.NodeDef -> IO ()
updateDefinition = defOperation (\batchHandler defID definition -> do
    putStrLn "call updateDefinition - NOT IMPLEMENTED, sorry."
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    return ())


removeDefinition :: IORef Core -> Maybe TDefs.NodeDef -> IO ()
removeDefinition = defOperation (\batchHandler defID _ -> do
    putStrLn "call removeDefinition"
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    case DefManager.gelem defID defManager of 
        True -> do let newCore = core{ Core.defManager= DefManager.delNode defID defManager }
                   writeIORef batchHandler newCore
        False -> throw $ ArgumentException $ Just $ pack "Wrong `defID` in `definition` field")


definitionChildren :: IORef Core -> Maybe TDefs.NodeDef -> IO (Vector TDefs.NodeDef)
definitionChildren = defOperation (\batchHandler defID _ -> do
    putStrLn "call definitionChildren"  
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    case DefManager.gelem defID defManager of 
        True -> do let children = DefManager.children defManager defID
                       tchildrenWithGraph = map (encode) children
                       tchildren = map (\(def, _) -> def) tchildrenWithGraph
                   return $ Vector.fromList tchildren
        False -> throw $ ArgumentException $ Just $ pack "Wrong `defID` in `definition` field")


definitionParent :: IORef Core -> Maybe TDefs.NodeDef -> IO TDefs.NodeDef
definitionParent = defOperation (\batchHandler defID _ -> do
    putStrLn "call definitionParent"
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    case DefManager.gelem defID defManager of 
        True -> do let parent = DefManager.parent defManager defID
                   case parent of 
                       Just p  -> do let (tparent, _) = encode p
                                     return tparent
                       Nothing -> -- TODO [PM] : what if there is no parent?
                                  undefined
        False -> throw $ ArgumentException $ Just $ pack "Wrong `defID` in `definition` field")

