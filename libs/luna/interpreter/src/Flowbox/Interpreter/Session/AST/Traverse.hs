---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.AST.Traverse (
  previous
, into
) where

import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List
import qualified Data.Maybe          as Maybe
import qualified GHC.Exts            as Exts

import           Flowbox.Control.Error
import qualified Flowbox.Interpreter.Session.Data.CallData     as CallData
import           Flowbox.Interpreter.Session.Data.CallDataPath (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallPoint    as CallPoint
import           Flowbox.Interpreter.Session.Data.DefPoint     (DefPoint (DefPoint))
import           Flowbox.Interpreter.Session.Session           (Session)
import qualified Flowbox.Interpreter.Session.Session           as Session
import           Flowbox.Luna.Data.Graph.Edge                  (Edge)
import qualified Flowbox.Luna.Data.Graph.Edge                  as Edge
import qualified Flowbox.Luna.Data.Graph.Graph                 as Graph
import           Flowbox.Luna.Data.Graph.Node                  (Node)
import qualified Flowbox.Luna.Data.Graph.Node                  as Node
import qualified Flowbox.Luna.Data.Graph.Port                  as Port
import qualified Flowbox.Luna.Passes.Analysis.NameResolver     as NameResolver
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Traverse"



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
globalPredecesors callDataPath (nodeID, node, edge) = case node of
    Node.Inputs -> do print (">>>>>>>>", nodeID, "prev")
                      matchPredecessor (init callDataPath) $ edge ^. Edge.src
    _        -> do print (">>>>>>>>", nodeID)
                   return $ Just $ init callDataPath ++ [last callDataPath & CallData.callPoint . CallPoint.nodeID .~ nodeID
                                                                           & CallData.node .~ node]

matchPredecessor :: CallDataPath -> Port.OutPort -> Session (Maybe CallDataPath)
matchPredecessor []            _                = return Nothing
matchPredecessor _             Port.All         = left "Incorrectly connected graph (1)"
matchPredecessor callDataPath (Port.Num portNo) = do
    let matching (_, _, edge) = edge ^. Edge.dst == portNo
    found <- List.find matching (localPredecessors callDataPath) <??> "Incorrectly connected graph (2)"
    globalPredecesors callDataPath found


into :: CallDataPath -> Session (Maybe DefPoint)
into callDataPath = do
    let callData  = last callDataPath
        libraryID = callData ^. CallData.callPoint . CallPoint.libraryID
        parentBC  = callData ^. CallData.parentBC
        node      = callData ^. CallData.node
    libManager <- Session.getLibManager
    results <- EitherT $ NameResolver.run (node ^. Node.expr) parentBC libraryID libManager
    case results of
        []       -> return   Nothing
        [result] -> return $ Just $ uncurry DefPoint result
        _        -> left "Name resolver returned multiple results"


--next :: CallDataPath -> Session [CallDataPath]
--next []           = return []
--next callDataPath = do
--    let graph = last callDataPath ^. CallData.parentGraph
--        successors = Graph.sucl graph $ last callDataPath ^. CallData.callPoint . CallPoint.nodeID
--    List.concat
