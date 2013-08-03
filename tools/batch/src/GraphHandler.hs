---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module GraphHandler (
graph,
addNode,
updateNode,
removeNode,
connect,
disconnect
) 
where

import           Control.Exception

import           Data.IORef
import           Data.Text.Lazy   (pack, unpack)
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)

import           Batch_Types (ArgumentException(..))
import           DefinitionHandler               (defOperation)
import qualified Defs_Types                    as TDefs
import qualified Graph_Types                   as TGraph
import qualified Luna.Core                     as Core
import           Luna.Core                       (Core)
import qualified Luna.Network.Def.DefManager   as DefManager
import qualified Luna.Network.Def.NodeDef      as NodeDef
import           Luna.Network.Def.NodeDef        (NodeDef)
import qualified Luna.Network.Graph.Graph      as Graph
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Graph   ()


graph :: IORef Core -> Maybe TDefs.NodeDef -> IO TGraph.Graph
graph = defOperation (\batchHandler defID _ ->
    do core <- readIORef batchHandler
       let defManager = Core.defManager core
       case DefManager.lab defManager defID  of 
           Just definition -> do putStrLn "called graph"
                                 let graph = NodeDef.graph definition
                                 return $ encode graph
           Nothing         -> throw $ ArgumentException $ Just $ pack "Wrong `defID` in `definition` field")


addNode batchHandler mtnode mtdefinition = do
    putStrLn "NOT IMPLEMENTED - addNode"
    return $ TGraph.Node Nothing Nothing Nothing Nothing Nothing Nothing


updateNode batchHandler mtnode mtdefinition = do
    putStrLn "NOT IMPLEMENTED - updateNode"


removeNode batchHandler mtnode mtdefinition = do
    putStrLn "NOT IMPLEMENTED - removeNode"


connect    batchHandler mtsrcNode mtsrcPort mtdstNode mtdstPort mtdefinition = do 
    putStrLn "NOT IMPLEMENTED - connect"


disconnect batchHandler mtsrcNode mtsrcPort mtdstNode mtdstPort mtdefinition = do 
    putStrLn "NOT IMPLEMENTED - disconnect"
