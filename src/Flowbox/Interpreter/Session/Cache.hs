---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Cache where

import           Control.Monad.State hiding (mapM_)
import qualified Data.List           as List

import           Flowbox.Control.Error
import qualified Flowbox.Data.MapForest                as MapForest
import           Flowbox.Interpreter.Session.CallPath  (CallPath)
import qualified Flowbox.Interpreter.Session.CallPath  as CallPath
import           Flowbox.Interpreter.Session.CallPoint (CallPoint)
import           Flowbox.Interpreter.Session.DefPath   (DefPath)
import           Flowbox.Interpreter.Session.DefPoint  (DefPoint)
import qualified Flowbox.Interpreter.Session.Env       as Env
import           Flowbox.Interpreter.Session.Session   (Session)
import qualified Flowbox.Interpreter.Session.Session   as Session
import qualified Flowbox.Luna.Data.AST.Zipper.Focus    as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper   as Zipper
import           Flowbox.Luna.Data.Graph.Graph         (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph         as Graph
import           Flowbox.Luna.Data.Graph.Node          (Node)
import qualified Flowbox.Luna.Data.Graph.Node          as Node
import qualified Flowbox.Luna.Lib.LibManager           as LibManager
import qualified Flowbox.Luna.Lib.Library              as Library
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache"



dump :: CallPath -> Session ()
dump callPath = do
    logger debug $ "Dumping " ++ show callPath
    Session.runStmt ("print " ++ argPrefix ++ show callPath)


invalidate :: CallPath -> Session ()
invalidate callPath = do
    modify (Env.cached %~ MapForest.delete callPath)
    let varName = CallPath.toVarName callPath
    logger debug $ "Invalidating " ++ show varName
    Session.runStmt (varName ++ " <- return ()")
    --mapM_ (invalidate graph) $ Node.suc graph nodeID


argPrefix :: String
argPrefix = "_"


previous :: CallPath -> Session [CallPath]
previous callPath = do
    undefined

next :: CallPath -> Session [CallPath]
next callPath = do
    undefined



findMain :: Session DefPoint
findMain = gets $ view Env.mainPtr


executeMain :: Session ()
executeMain = findMain >>= executeGraph []


executeGraph :: CallPath -> DefPoint -> Session ()
executeGraph callPath defPoint = do
    graph <- Session.getGraph defPoint
    mapM_ (runNodeIfNeeded callPath (fst defPoint) graph) $ Graph.labNodes graph


runNodeIfNeeded ::  CallPath -> Library.ID -> Graph -> (Node.ID, Node) -> Session ()
runNodeIfNeeded callPath libraryID graph n =
    unlessM (isCached $ callPath ++ [(libraryID, fst n)])
            (runNode  callPath libraryID graph n)


runNode :: CallPath -> Library.ID -> Graph -> (Node.ID, Node) -> Session ()
runNode callPath libraryID graph (nodeID, node) = do
    let predecessors = Graph.prel graph nodeID
    mapM_ (runNodeIfNeeded callPath libraryID graph) predecessors

    logger debug $ show node
    ----let functionName = node ^. Node.code
    ----    functionType = node ^. Node.cls . Type.repr
    ----    args         = map (\i -> argPrefix ++ show i) predecessors
    ----    function     = "toIO $ extract $ (Operation (" ++ functionName ++ " :: " ++ functionType ++ "))"
    ----    argSeparator = " `call` "
    ----    operation    = List.intercalate argSeparator (function : args)
    ----    expression   = argPrefix ++ show nodeID ++ " <- " ++ operation
    ----Session.runStmt expression
    modify (Env.cached %~ MapForest.insert (callPath ++ [(libraryID, nodeID)]))
    logger trace =<< show <$> gets (view Env.cached)


isCached :: CallPath -> Session Bool
isCached callPath = MapForest.member callPath <$> gets (view Env.cached)
