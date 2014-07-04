---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.AST.Traverse (
  into
, next
, previous
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
import           Flowbox.Prelude                               hiding (inside)
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



next :: CallDataPath -> Session [CallDataPath]
next []           = return []
next callDataPath = Maybe.catMaybes
    <$> mapM (globalSuccessors callDataPath)
             (localSuccessors callDataPath)

localSuccessors :: CallDataPath -> [(Node.ID, Node)]
localSuccessors callDataPath = localSuccs where
    graph      = last callDataPath ^. CallData.parentGraph
    localSuccs = Graph.sucl graph $ last callDataPath ^. CallData.callPoint . CallPoint.nodeID


globalSuccessors :: CallDataPath -> (Node.ID, Node) -> Session (Maybe CallDataPath)
globalSuccessors []           _              = return Nothing
globalSuccessors callDataPath (nodeID, node) = do
    let upperLevel = init callDataPath
        thisLevel  = last callDataPath
    case node of
        Node.Outputs -> if null callDataPath
            then return Nothing
            else do let parentNode = ( thisLevel ^. CallData.callPoint . CallPoint.nodeID
                                     , thisLevel ^. CallData.node)
                    globalSuccessors upperLevel parentNode
        _            -> return $ Just $ upperLevel ++ [thisLevel
                            & CallData.callPoint . CallPoint.nodeID .~ nodeID
                            & CallData.node .~ node]
