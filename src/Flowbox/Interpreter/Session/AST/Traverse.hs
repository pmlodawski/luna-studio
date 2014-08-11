---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.AST.Traverse (
  arguments
, inDataConnections
, into
-- , nextLocal
, up
, next
) where

import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List
import qualified Data.Maybe          as Maybe
import qualified GHC.Exts            as Exts

import           Flowbox.Control.Error
import qualified Flowbox.Interpreter.Session.AST.Inspect       as Inspect
import qualified Flowbox.Interpreter.Session.Data.CallData     as CallData
import           Flowbox.Interpreter.Session.Data.CallDataPath (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallDataPath as CallDataPath
import qualified Flowbox.Interpreter.Session.Data.CallPoint    as CallPoint
import           Flowbox.Interpreter.Session.Session           (Session)
import           Flowbox.Luna.Data.Graph.Edge                  (Edge)
import qualified Flowbox.Luna.Data.Graph.Edge                  as Edge
import qualified Flowbox.Luna.Data.Graph.Graph                 as Graph
import           Flowbox.Luna.Data.Graph.Node                  (Node)
import qualified Flowbox.Luna.Data.Graph.Node                  as Node
import           Flowbox.Prelude                               hiding (inside, matching)



arguments :: CallDataPath -> Session [CallDataPath]
arguments []           = return []
arguments callDataPath =
    Maybe.catMaybes <$> mapM (globalPredecessor callDataPath)
                             (Exts.sortWith (\edge -> edge ^. _3 ^? Edge.dst)
                                            (inDataConnections callDataPath))


inDataConnections :: CallDataPath -> [(Node.ID, Node, Edge)]
inDataConnections callDataPath = Graph.lprelData graph nodeID where
    graph  = last callDataPath ^. CallData.parentGraph
    nodeID = last callDataPath ^. CallData.callPoint . CallPoint.nodeID


globalPredecessor :: CallDataPath -> (Node.ID, Node, Edge) -> Session (Maybe CallDataPath)
globalPredecessor []           _                    = return Nothing
globalPredecessor callDataPath (nodeID, node, edge) = do
    let upperLevel = init callDataPath
    case node of
        Node.Inputs {} -> if null upperLevel
                            then return Nothing
                            else do found <- matchPredecessor upperLevel edge
                                    globalPredecessor upperLevel found
        _           -> return $ Just $ CallDataPath.updateNode callDataPath node nodeID


matchPredecessor :: CallDataPath -> Edge -> Session (Node.ID, Node, Edge)
matchPredecessor callDataPath edge =
    List.find (flip Edge.match edge . view _3) (inDataConnections callDataPath) <??> "Incorrectly connected graph"


into :: CallDataPath -> Session [CallDataPath]
into callDataPath = do
    let callData  = last callDataPath
        libraryID = callData ^. CallData.callPoint . CallPoint.libraryID
        parentBC  = callData ^. CallData.parentBC
        node      = callData ^. CallData.node
    mdefPoint <- Inspect.fromName (node ^. Node.expr) parentBC libraryID
    case mdefPoint of
        Nothing       -> return []
        Just defPoint -> CallDataPath.addLevel callDataPath defPoint


up :: CallDataPath -> CallDataPath
up = init


next :: CallDataPath -> Session [CallDataPath]
next []           = return []
next callDataPath = do
    let callData  = last callDataPath
        graph  = callData ^. CallData.parentGraph
        nodeID = callData ^. CallData.callPoint . CallPoint.nodeID
        sucl   = Graph.sucl graph nodeID
    concat <$> mapM (globalSuccessors callDataPath) sucl


globalSuccessors :: CallDataPath -> (Node.ID, Node)  -> Session [CallDataPath]
globalSuccessors prevCallDataPath (nodeID, node) = do
    let callDataPath = CallDataPath.updateNode prevCallDataPath node nodeID
    inner <- into callDataPath
    return $ callDataPath : inner
