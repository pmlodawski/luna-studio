---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.AST.Traverse (
  into
, nextLocal
, previous
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
import qualified Flowbox.Luna.Data.Graph.Port                  as Port
import           Flowbox.Prelude                               hiding (inside, matching)



previous :: CallDataPath -> Session [CallDataPath]
previous []           = return []
previous callDataPath =
    Maybe.catMaybes <$> mapM (globalPredecesors callDataPath)
                             (Exts.sortWith (view $ _3 . Edge.dst)
                                            (localPredecessors callDataPath))

localPredecessors :: CallDataPath -> [(Node.ID, Node, Edge)]
localPredecessors callDataPath = localPreds where
    graph      = last callDataPath ^. CallData.parentGraph
    localPreds = Graph.lprel graph $ last callDataPath ^. CallData.callPoint . CallPoint.nodeID


globalPredecesors :: CallDataPath -> (Node.ID, Node, Edge) -> Session (Maybe CallDataPath)
globalPredecesors []           _                    = return Nothing
globalPredecesors callDataPath (nodeID, node, edge) = do
    let upperLevel = (init callDataPath)
        thisLevel  = (last callDataPath)
    case node of
        Node.Inputs -> if null upperLevel
                        then return Nothing
                        else do found <- matchPredecessor upperLevel $ edge ^. Edge.src
                                globalPredecesors upperLevel found
        _           -> return $ Just $ upperLevel ++ [thisLevel
                           & CallData.callPoint . CallPoint.nodeID .~ nodeID
                           & CallData.node .~ node]


matchPredecessor :: CallDataPath -> Port.OutPort -> Session (Node.ID, Node, Edge)
matchPredecessor _             Port.All         = left "Incorrectly connected graph (1)"
matchPredecessor callDataPath (Port.Num portNo) = do
    let matching (_, _, edge) = edge ^. Edge.dst == portNo
    List.find matching (localPredecessors callDataPath) <??> "Incorrectly connected graph (2)"


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


nextLocal :: CallDataPath -> [CallDataPath]
nextLocal []           = []
nextLocal callDataPath = localSuccs where
    graph      = last callDataPath ^. CallData.parentGraph
    nodeID'    = last callDataPath ^. CallData.callPoint . CallPoint.nodeID
    succNodes  = Graph.sucl graph nodeID'
    upperLevel = (init callDataPath)
    thisLevel  = (last callDataPath)
    localSuccs = map (\(nodeID, node) -> upperLevel ++ [thisLevel
                                        & CallData.callPoint . CallPoint.nodeID .~ nodeID
                                        & CallData.node .~ node]) succNodes

up :: CallDataPath -> CallDataPath
up = init


next :: CallDataPath -> Session [CallDataPath]
next []           = return []
next callDataPath = do
    let callData  = last callDataPath
        graph  = callData ^. CallData.parentGraph
        nodeID = callData ^. CallData.callPoint . CallPoint.nodeID
        node   = callData ^. CallData.node
        sucl   = Graph.sucl graph nodeID
    case node of
        Node.Outputs -> next $ init callDataPath
        _            -> concat <$> mapM (globalSuccessors callDataPath) sucl


globalSuccessors :: CallDataPath -> (Node.ID, Node)  -> Session [CallDataPath]
globalSuccessors callDataPath (nodeID, node) = do
    let callData  = last callDataPath
    inner <- into callDataPath
    if not $ null inner
        then return inner
        else return [init callDataPath
                    ++ [callData & (CallData.callPoint . CallPoint.nodeID .~ nodeID)
                                 & (CallData.node .~ node)]]

