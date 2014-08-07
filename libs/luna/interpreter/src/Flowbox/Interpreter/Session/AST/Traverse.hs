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
import qualified Flowbox.Luna.Data.Graph.Port                  as Port
import           Flowbox.Prelude                               hiding (inside, matching)



arguments :: CallDataPath -> Session [CallDataPath]
arguments []           = return []
arguments callDataPath =
    Maybe.catMaybes <$> mapM (globalPredecesor callDataPath)
                             (Exts.sortWith (\edge -> edge ^. _3 ^? Edge.dst)
                                            (inDataConnections callDataPath))


inDataConnections :: CallDataPath -> [(Node.ID, Node, Edge)]
inDataConnections callDataPath = localPreds where
    graph           = last callDataPath ^. CallData.parentGraph
    isDataEdge edge = Edge.isData $ edge ^. _3
    localPreds      = List.filter isDataEdge
                    $ Graph.lprel graph
                    $ last callDataPath ^. CallData.callPoint . CallPoint.nodeID


globalPredecesor :: CallDataPath -> (Node.ID, Node, Edge) -> Session (Maybe CallDataPath)
globalPredecesor []           _                    = return Nothing
globalPredecesor callDataPath (nodeID, node, edge) = do
    let upperLevel = init callDataPath
    case node of
        Node.Inputs {} -> if null upperLevel
                            then return Nothing
                            else do found <- matchPredecessor upperLevel edge
                                    globalPredecesor upperLevel found
        _           -> return $ Just $ CallDataPath.updateNode callDataPath node nodeID


matchPredecessor :: CallDataPath -> Edge -> Session (Node.ID, Node, Edge)
matchPredecessor callDataPath edge = do
    let matching (Edge.Data src _) (_, _, Edge.Data _ dst) = src == Port.Num dst
        --matching  Edge.Monadic     (_, _, Edge.Monadic   ) = True
        matching  _                 _                      = False
    List.find (matching edge) (inDataConnections callDataPath) <??> "Incorrectly connected graph"


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
